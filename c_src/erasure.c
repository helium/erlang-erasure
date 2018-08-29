/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "erl_nif.h"
#include <stdbool.h>
#include <string.h>
#include <jerasure.h>
#include <reed_sol.h>

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;


static ERL_NIF_TERM
encode(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int k;
    if (!enif_get_int(env, argv[0], &k))
    {
        return enif_make_badarg(env);
    }

    int m;
    if (!enif_get_int(env, argv[1], &m))
    {
        return enif_make_badarg(env);
    }

    ErlNifBinary input;
    if (!enif_is_binary(env, argv[2]) || !enif_inspect_binary(env, argv[2], &input))
    {
        return enif_make_badarg(env);
    }

    int blocksize = input.size / k;
    int padding = 0;
    int remainder = input.size % k;

    if (remainder != 0) {
        // payload is not cleanly divible by K, we need to pad
        padding = (k - (input.size % k));
        blocksize = (input.size + padding) / k;
        while (blocksize % sizeof(long) != 0) {
            blocksize++;
            padding++;
        }
    }

    // block spacing has to be a multiple of 16
    int blockspacing = blocksize + (16 - (blocksize % 16));

    int bytes_per_shard = input.size / k;
    int extra_bytes = input.size % k;

    char *shards = calloc(k+m, blockspacing);
    char *data_ptrs[k];
    char *coding_ptrs[m];

    unsigned char *p = input.data;
    for (int i = 0; i < k+m; i++) {
        memset(shards+(blockspacing*i), 0, blockspacing);
        if (i < k) {
            data_ptrs[i] = shards+(blockspacing*i);
            memcpy(shards+(blockspacing*i), p, bytes_per_shard);
            p += bytes_per_shard;
            if (extra_bytes > 0) {
                memcpy(shards+(blockspacing*i)+bytes_per_shard, p, 1);
                p++;
                extra_bytes--;
            }
        } else {
            coding_ptrs[i-k] = shards+(blockspacing*i);
        }
    }


    int w = 8;
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    jerasure_matrix_encode(k, m, w, matrix, data_ptrs, coding_ptrs, blocksize);

    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (int i = k+m - 1; i >= 0; i--) 
    {
        ERL_NIF_TERM binary;
        unsigned char *bindata = enif_make_new_binary(env, blocksize, &binary);
        memcpy(bindata, shards+(blockspacing*i), blocksize);
        list = enif_make_list_cell(env,
                enif_make_tuple3(env,
                    enif_make_int(env, i),
                    enif_make_int(env, input.size),
                    binary
                    ), list);
    }
    free(shards);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), list);
}

static ERL_NIF_TERM
decode(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int k;
    ERL_NIF_TERM result;
    if (!enif_get_int(env, argv[0], &k))
    {
        return enif_make_badarg(env);
    }

    int m;
    if (!enif_get_int(env, argv[1], &m))
    {
        return enif_make_badarg(env);
    }

    unsigned len;
    if (!enif_is_list(env, argv[2]) || !enif_get_list_length(env, argv[2], &len) || len < k)
    {
        // need at least K shards, sorry
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "insufficent_shards"));
    }

    char *shards = NULL;
    char *data_ptrs[k];
    char *coding_ptrs[m];

    int *erasures = NULL;

    for (int i = 0; i < k; i++) {
        data_ptrs[i] = NULL;
    }

    for (int i = 0; i < m; i++) {
        coding_ptrs[i] = NULL;
    }

    // all the shards must be the same size
    // and all the indices need to be in-bounds
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tuple;
    int arity, id, totalsize, lasttotalsize = 0;
    int padding, blocksize = 0, remainder=0, blockspacing=0;
    tail = argv[2];
    while (enif_get_list_cell(env, tail, &head, &tail))
    {
        if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 3) {
            result = enif_make_badarg(env);
            goto cleanup;
        }
        if (!enif_get_int(env, tuple[0], &id) || id < 0 || id >= k+m) {
            result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_shard_id"));
            goto cleanup;
        }
        if ((id < k && data_ptrs[id] != NULL) || ( id >=k && coding_ptrs[id-k] != NULL)) {
            result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "duplicate_shard_id"));
            goto cleanup;
        }
        if (!enif_get_int(env, tuple[1], &totalsize) || totalsize <= 0) {
            result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_total_size"));
            goto cleanup;
        }
        if (lasttotalsize != 0 && totalsize != lasttotalsize) {
            result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "inconsistent_total_size"));
            goto cleanup;
        }
        lasttotalsize=totalsize;

        if (blocksize == 0) {
            blocksize = totalsize / k;
            padding = 0;

            remainder = totalsize % k;
            if (remainder != 0) {
                // payload is not cleanly divible by K, we need to pad
                padding = (k - (totalsize % k));
                blocksize = (totalsize + padding) / k;
                while (blocksize % sizeof(long) != 0) {
                    blocksize++;
                    padding++;
                }
            }
            // block spacing has to be a multiple of 16
            blockspacing = blocksize + (16 - (blocksize % 16));
            shards = calloc(k+m, blockspacing);
        }

        ErlNifBinary input;
        if (!enif_is_binary(env, tuple[2]) || !enif_inspect_binary(env, tuple[2], &input) || input.size != blocksize)
        {
            result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_shard"));
            goto cleanup;
        }

        if (id < k) {
            data_ptrs[id] = shards+(blockspacing*id);
        } else {
            coding_ptrs[id-k] = shards+(blockspacing*id);
        }

        memset(shards+(blockspacing*id), 0, blockspacing);
        memcpy(shards+(blockspacing*id), input.data, blocksize);
    }

    erasures = calloc(k+m, sizeof(int));
    int j = 0;

    int bytes_per_shard = totalsize / k;
    int extra_bytes = totalsize % k;

    // calculate the missing shards and track them in the erasures array
    for (int i = 0; i < k+m; i++) {
        if ((i < k && data_ptrs[i] == NULL) || (i >= k && coding_ptrs[i-k] == NULL)) {
            // set up any missing data or coding pointers
            if (i < k && data_ptrs[i] == NULL) {
                data_ptrs[i] = shards+(blockspacing*i);
            } else if (i >= k && coding_ptrs[i-k] == NULL) {
                coding_ptrs[i-k] = shards+(blockspacing*i);
            }
            erasures[j] = i;
            j++;
        }
    }
    erasures[j] = -1;

    int w = 8;
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    int res = jerasure_matrix_decode(k, m, w, matrix, 1, erasures, data_ptrs, coding_ptrs, blocksize);
    //abort();

    if (res == -1) {
        result = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "decode_failed"));
        goto cleanup;
    }

    ERL_NIF_TERM decoded;
    unsigned char* decoded_data = enif_make_new_binary(env, totalsize, &decoded);
    memset(decoded_data, 0, totalsize);
    unsigned char *p = decoded_data;

    for (int i = 0; i < k; i++) {
        memcpy(p, shards+(blockspacing*i), bytes_per_shard);
        p += bytes_per_shard;
        if (extra_bytes > 0) {
            memcpy(p, shards+(blockspacing*i)+bytes_per_shard, 1);
            extra_bytes--;
            p++;
        }
    }

    result = enif_make_tuple2(env, enif_make_atom(env, "ok"), decoded);

cleanup:

    if (shards != NULL) {
        free(shards);
    }
    if (erasures != NULL) {
        free(erasures);
    }

    return result;
}

static ErlNifFunc nif_funcs[] =
    {{"encode", 3, encode, 0},
     {"decode", 3, decode, 0}};

#define ATOM(Id, Value)                                                        \
    {                                                                          \
        Id = enif_make_atom(env, Value);                                       \
    }

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");

    return 0;
}

ERL_NIF_INIT(erasure, nif_funcs, load, NULL, NULL, NULL);

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


    char *shards[k+m];

    for (int i = 0; i < k+m; i++) {
        shards[i] = (char *)malloc(sizeof(char)*blocksize);
        memset(shards[i], 0, blocksize);
        if (i < k) {
            if (i == 0) {
                memcpy(shards[i]+remainder, input.data+(i*(blocksize)), blocksize - remainder);
            } else {
                memcpy(shards[i], input.data-remainder+(i*(blocksize)), blocksize);
            }
        }
    }

    int w = 8;
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    jerasure_matrix_encode(k, m, w, matrix, shards, shards+k, blocksize);

    ERL_NIF_TERM list = enif_make_list(env, 0);

    ErlNifBinary binary;
    for (int i = k+m - 1; i >= 0; i--) 
    {
        enif_alloc_binary(blocksize, &binary);
        memcpy(binary.data, shards[i], blocksize);
        free(shards[i]);
        list = enif_make_list_cell(env,
                enif_make_tuple3(env,
                    enif_make_int(env, i),
                    enif_make_int(env, input.size),
                    enif_make_binary(env, &binary)
                    ), list);
    }
    return list;
}

static ERL_NIF_TERM
decode(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
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

    unsigned len;
    if (!enif_is_list(env, argv[2]) || !enif_get_list_length(env, argv[2], &len) || len < k)
    {
        // need at least K shards, sorry
        // TODO return a better error here
        printf("too few shards\n");
        return enif_make_badarg(env);
    }

    char *shards[k+m];

    for (int i = 0; i < k+m; i++) {
        shards[i] = NULL;
        /*memset(shards[i], 0, blocksize);*/
    }

    // all the shards must be the same size
    // and all the indices need to be in-bounds
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tuple;
    int arity, id, totalsize, lasttotalsize = 0;
    int padding, blocksize = 0, remainder=0;
    tail = argv[2];
    while (enif_get_list_cell(env, tail, &head, &tail))
    {
        if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 3) {
            // TODO cleanup
            printf("bad arity\n");
            return enif_make_badarg(env);
        }
        if (!enif_get_int(env, tuple[0], &id) || id < 0 || id >= k+m) { // || shards[id] != NULL) {
            // TODO cleanup
            printf("bad id %d\n", id);
            return enif_make_badarg(env);
        }
        if (!enif_get_int(env, tuple[1], &totalsize) || totalsize <= 0) {
            // TODO cleanup
            printf("bad totalsize\n");
            return enif_make_badarg(env);
        }
        if (lasttotalsize != 0 && totalsize != lasttotalsize) {
            // TODO cleanup
            printf("inconsistent totalsize\n");
            return enif_make_badarg(env);
        }
        /*printf("lol %d\n", id);*/
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
        }
        shards[id] = (char *)malloc(sizeof(char)*blocksize);
        memset(shards[id], 0, blocksize);

        ErlNifBinary input;
        if (!enif_is_binary(env, tuple[2]) || !enif_inspect_binary(env, tuple[2], &input))
        {
            printf("bad shard\n");
            return enif_make_badarg(env);
        }
        memcpy(shards[id], input.data, blocksize);
    }

    int erasures[k+m];
    int j = 0;

    // calculate the missing shards and fill them in with 0s
    for (int i = 0; i < k+m; i++) {
        if (shards[i] == NULL) {
            erasures[j] = i;
            j++;
            shards[i] = (char *)malloc(sizeof(char)*blocksize);
            memset(shards[i], 0, blocksize);
        }
    }
    erasures[j] = -1;

    int w = 8;
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    int res = jerasure_matrix_decode(k, m, w, matrix, 1, erasures, shards, shards+k, blocksize);

    if (res == -1) {
        // TODO cleanup
        printf("decode failed\n");
        return enif_make_badarg(env);
    }


    ErlNifBinary result;
    enif_alloc_binary(totalsize, &result);

    for (int i = 0; i < k; i++) {
        if (i == 0) {
            memcpy(result.data, shards[i]+remainder, blocksize - remainder);
        } else {
            memcpy(result.data-remainder+(i*blocksize), shards[i], blocksize);
        }
    }

    return enif_make_binary(env, &result);
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

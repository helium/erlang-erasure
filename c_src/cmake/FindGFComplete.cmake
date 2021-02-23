include(ExternalProject)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

if (APPLE)
  execute_process(COMMAND
    xcrun --show-sdk-path
    OUTPUT_VARIABLE APPLE_SDK_ROOT
    )
  set(GF_APPLE_ENV "SDKROOT=${APPLE_SDK_ROOT}")
endif()

ExternalProject_Add(gf-complete
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/gf-complete
  GIT_REPOSITORY    https://github.com/ceph/gf-complete.git
  GIT_TAG           a6862d10c9db467148f20eef2c6445ac9afd94d8
  UPDATE_COMMAND    ""
  BUILD_IN_SOURCE   1
  CONFIGURE_COMMAND autoreconf --install > /dev/null 2>&1 || autoreconf &&
                    ./configure
                    --prefix=${CMAKE_CURRENT_BINARY_DIR}
                    --with-pic
                    --disable-shared
                    $ENV{CONFIGURE_ARGS}
                    CC=${CMAKE_C_COMPILER}
                    CFLAGS=${CMAKE_C_FLAGS_${BUILD_TYPE_UC}}
                    ${GF_APPLE_ENV}  
  BUILD_COMMAND     ${CMAKE_BUILD_TOOL} -j ${GF_APPLE_ENV}
  BUILD_BYPRODUCTS  ${CMAKE_CURRENT_BINARY_DIR}/lib/libgf_complete.a
  INSTALL_COMMAND   ${CMAKE_BUILD_TOOL} install
  )

add_library(GFComplete::GFComplete STATIC IMPORTED)
set_target_properties(GFComplete::GFComplete
  PROPERTIES
  IMPORTED_LOCATION             ${CMAKE_CURRENT_BINARY_DIR}/lib/libgf_complete.a
  INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}/include
  )
add_dependencies(GFComplete::GFComplete gf-complete)

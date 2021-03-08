include(FindPackageHandleStandardArgs)
include(ExternalProject)

find_package(GFComplete REQUIRED)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

set(CONFIGURE_ARGS $ENV{CONFIGURE_ARGS})
separate_arguments(CONFIGURE_ARGS)

set(CONFIGURE_CFLAGS   "$ENV{CFLAGS}   -I${CMAKE_CURRENT_BINARY_DIR}/include ${CMAKE_C_FLAGS_${BUILD_TYPE_UC}}")
set(CONFIGURE_LDFLAGS  "$ENV{LDFLAGS}  -L${CMAKE_CURRENT_BINARY_DIR}/lib")

ExternalProject_Add(jerasure
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/jerasure
  GIT_REPOSITORY    http://github.com/ceph/jerasure.git
  GIT_TAG           de1739cc8483696506829b52e7fda4f6bb195e6a
  UPDATE_COMMAND    ""
  BUILD_IN_SOURCE   1
  CONFIGURE_COMMAND autoreconf --install > /dev/null 2>&1 || autoreconf &&
                    ./configure
                    --prefix=${CMAKE_CURRENT_BINARY_DIR}
                    --with-pic
                    --disable-shared
                    ${CONFIGURE_ARGS}
                    CC=${CMAKE_C_COMPILER}
                    CFLAGS=${CONFIGURE_CFLAGS}
                    LDFLAGS=${CONFIGURE_LDFLAGS}
                    ${GF_APPLE_ENV}
  BUILD_COMMAND     ${CMAKE_BUILD_TOOL} -j ${GF_APPLE_ENV}
  BUILD_BYPRODUCTS  ${CMAKE_CURRENT_BINARY_DIR}/lib/libJerasure.a
  INSTALL_COMMAND   ${CMAKE_BUILD_TOOL} install
  )
ExternalProject_Add_StepDependencies(jerasure build gf-complete)

# Hack to let us declare a not-yet-existing include path below.
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/jerasure)

add_library(Jerasure::Jerasure STATIC IMPORTED)
target_include_directories(Jerasure::Jerasure
  INTERFACE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_BINARY_DIR}/include/jerasure
  )
target_link_libraries(Jerasure::Jerasure
  INTERFACE
  GFComplete::GFComplete
  )
set_target_properties(Jerasure::Jerasure
  PROPERTIES
  IMPORTED_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/lib/libJerasure.a
  )
add_dependencies(Jerasure::Jerasure jerasure)

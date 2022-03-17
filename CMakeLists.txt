cmake_minimum_required(VERSION 3.14)
project(ustring)

include (CMakeDependentOption)

cmake_dependent_option (
    USTRING_BUILD_TESTS
    "Build the test suite"
    ON
    "CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME"
    OFF
)

add_library (ustring INTERFACE)

target_include_directories (
    ustring
    INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

target_compile_features (
    ustring
    INTERFACE
    cxx_std_20
)

if (USTRING_BUILD_TESTS)
    include (CTest)

    add_subdirectory (tests)
endif ()

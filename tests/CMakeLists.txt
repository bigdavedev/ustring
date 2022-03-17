add_executable (test_ustring test_ustring.cpp)

target_link_libraries (test_ustring PRIVATE ustring)

set_target_properties (
    test_ustring
    PROPERTIES
    CXX_EXTENSIONS OFF
)

add_test (test_ustring test_ustring)

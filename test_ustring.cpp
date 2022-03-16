


#include "ustring.h"

#include <iostream>

void test_ustring()
{
    using namespace std::literals::string_literals;

    std::ustring a;
    std::ustring b{ "b" };  // Can't init with = without trailing u as the ctor from character pointer is explicit. b will contain a copy of the literal.
    std::ustring c = "c"u;  // Here the literal is not copied thanks to the u suffix.
    std::ustring d = L"c"u;
    assert(c == d);

    std::ustring e = b + "hej"u;
    std::ustring g = char32_t('!');
    std::ustring f = e + g;

    std::cout << f.string() << std::endl;
}


int main()
{
    test_ustring();
}

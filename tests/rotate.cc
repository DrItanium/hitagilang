// this is meant for native cpu execution for testing purposes
#include <bit>
#include <cmath>
#include <cstdint>
#include <iostream>

using Ordinal = uint32_t;

int main() {
    for (Ordinal len = 0; len < 32; ++len) {
        for (Ordinal src = 0; src < 32; ++src) {
            std::cout << "rotate " << std::dec << len << ", " << std::dec << src << ", g0 -> 0x" << std::hex << std::rotl<Ordinal>(src, len) << std::endl;
        }
    }
    return 0;
}

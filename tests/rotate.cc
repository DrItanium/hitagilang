// this is meant for native cpu execution for testing purposes
#include <bit>
#include <cmath>
#include <cstdint>
#include <iostream>

using Ordinal = uint32_t;

int main() {
    for (Ordinal len = 0; len < 32; ++len) {
        for (Ordinal src = 0; src < 32; ++src) {
            //std::cout << "1 rotate " << std::dec << len << ", " << std::dec << src << ", g0 -> 0x" << std::hex << ((src << len) | (src >> ((-len) & 31u))) << std::endl;
            std::cout << "0 rotate " << std::dec << len << ", " << std::dec << src << ", g0 -> 0x" << std::hex << std::rotl<Ordinal>(src, len) << std::endl;
            std::cout << "\tp0: 0x" << std::hex << (src << len) << std::endl;
            std::cout << "\tp1: 0x" << std::hex << (src >> ((-len) & 31u)) << std::endl;
        }
    }
    return 0;
}

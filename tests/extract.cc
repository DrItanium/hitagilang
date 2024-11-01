

template<typename R, typename T>
R extractValue(T value, T mask, T shift) {
    return static_cast<R>((value >> shift) & mask);
}
template<typename R, typename T, T mask, T shift>
R extractValue(T value) {
    return static_cast<R>((value >> shift) & mask);
}

char extractLowest(int value) {
    return extractValue<int, char>(value, 0xFF, 0);
}
char extractLower(int value) {
    return extractValue<int, char>(value, 0xFF, 8);
}
char extractHigher(int value) {
    return extractValue<int, char>(value, 0xFF, 16);
}

char extractHighest(int value) {
    return extractValue<int, char>(value, 0xFF, 24);
}
unsigned char extractSrcDest(int value) {
    return extractValue<unsigned char, int, 0x1F, 19 >(value);
}

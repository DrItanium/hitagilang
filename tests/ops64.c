/*
 * Code to express 64-bit operations from the C programming language
 */

#include <stdint.h>

typedef uint64_t LongOrdinal;
typedef int64_t LongInteger;
typedef double LongReal;

LongOrdinal addol(LongOrdinal a, LongOrdinal b) { return a + b; }
LongOrdinal subol(LongOrdinal a, LongOrdinal b) { return a - b; }
LongOrdinal mulol(LongOrdinal a, LongOrdinal b) { return a * b; }
LongOrdinal divol(LongOrdinal a, LongOrdinal b) { return a / b; }
LongOrdinal remol(LongOrdinal a, LongOrdinal b) { return a % b; }
LongOrdinal shlol(LongOrdinal a, LongOrdinal b) { return a << b; }
LongOrdinal shrol(LongOrdinal a, LongOrdinal b) { return a >> b; }
LongOrdinal andl(LongOrdinal a, LongOrdinal b) { return a & b; }
LongOrdinal orl(LongOrdinal a, LongOrdinal b) { return a | b; }
LongOrdinal notl(LongOrdinal a) { return ~a; }
LongOrdinal xorl(LongOrdinal a, LongOrdinal b) { return a ^ b; }
LongOrdinal xnorl(LongOrdinal a, LongOrdinal b) { return ~(a ^ b); }

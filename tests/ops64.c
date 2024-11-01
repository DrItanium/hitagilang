/*
 * Code to express 64-bit operations from the C programming language
 */

#include <stdint.h>

typedef uint32_t Ordinal;
typedef int32_t Integer;
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
LongOrdinal xnorl(LongOrdinal a, LongOrdinal b) { return ~(b ^ a); }
LongOrdinal nandl(LongOrdinal a, LongOrdinal b) { return ~(b & a); }
LongOrdinal norl(LongOrdinal a, LongOrdinal b) { return ~(b | a); }

LongInteger addil(LongInteger a, LongInteger b) { return a + b; }
LongInteger subil(LongInteger a, LongInteger b) { return a - b; }
LongInteger mulil(LongInteger a, LongInteger b) { return a * b; }
LongInteger divil(LongInteger a, LongInteger b) { return a / b; }
LongInteger remil(LongInteger a, LongInteger b) { return a % b; }
LongInteger shlil(LongInteger a, LongInteger b) { return a << b; }
LongInteger shril(LongInteger a, LongInteger b) { return a >> b; }

Ordinal addo(Ordinal a, Ordinal b) { return a + b; }
Ordinal subo(Ordinal a, Ordinal b) { return a - b; }
Ordinal mulo(Ordinal a, Ordinal b) { return a * b; }
Ordinal divo(Ordinal a, Ordinal b) { return a / b; }
Ordinal remo(Ordinal a, Ordinal b) { return a % b; }
Ordinal shlo(Ordinal a, Ordinal b) { return a << b; }
Ordinal shro(Ordinal a, Ordinal b) { return a >> b; }
Ordinal and(Ordinal a, Ordinal b) { return a & b; }
Ordinal or(Ordinal a, Ordinal b) { return a | b; }
Ordinal not(Ordinal a) { return ~a; }
Ordinal xor(Ordinal a, Ordinal b) { return a ^ b; }
Ordinal xnor(Ordinal a, Ordinal b) { return ~(b ^ a); }
Ordinal nand(Ordinal a, Ordinal b) { return ~(b & a); }
Ordinal nor(Ordinal a, Ordinal b) { return ~(b | a); }

Integer addi(Integer a, Integer b) { return a + b; }
Integer subi(Integer a, Integer b) { return a - b; }
Integer muli(Integer a, Integer b) { return a * b; }
Integer divi(Integer a, Integer b) { return a / b; }
Integer remi(Integer a, Integer b) { return a % b; }
Integer shli(Integer a, Integer b) { return a << b; }
Integer shri(Integer a, Integer b) { return a >> b; }

int addi2(int a, int b) { return a + b; }
int subi2(int a, int b) { return a - b; }
int muli2(int a, int b) { return a * b; }
int divi2(int a, int b) { return a / b; }
int remi2(int a, int b) { return a % b; }
int shli2(int a, int b) { return a << b; }
int shri2(int a, int b) { return a >> b; }

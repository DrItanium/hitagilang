/*
 * Code to express 64-bit operations from the C programming language
 */

#include <stdint.h>
#include <stdbool.h>

typedef uint32_t Ordinal;
typedef int32_t Integer;
typedef uint64_t LongOrdinal;
typedef int64_t LongInteger;
typedef float Real;
typedef double LongReal;
typedef long double ExtendedReal;
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

LongOrdinal ldol(LongOrdinal* a) { return *a; }
LongInteger ldil(LongInteger* a) { return *a; }

Ordinal setbit(Ordinal value, int shift) { return (value) | (1 << (shift)); }
Ordinal clearbit(Ordinal value, int shift) { return (value) & (~(1 << (shift))); }

// structures
struct B {
    Ordinal a;
    Ordinal b;
    Ordinal c;
    Ordinal d;
};
struct C {
    Ordinal a;
    LongOrdinal b;
};
struct A {
    Ordinal a;
    LongOrdinal b;
    struct B c;
    struct C d;
};


Ordinal getA_a(struct A* a) { return a->a; }
LongOrdinal getA_b(struct A* a) { return a->b; }
struct B getA_c(struct A* a) { return a->c; }
struct C getA_d(struct A* a) { return a->d; }

LongOrdinal countItems(Ordinal* items) {
    LongOrdinal count = 0;
    for (Ordinal* curr = items; *curr; ++curr) {
        ++count;
    }
    return count;
}

struct B B_or(struct B a, struct B b) {
    struct B result;
    result.a = a.a | b.a;
    result.b = a.b | b.b;
    result.c = a.c | b.c;
    result.d = a.d | b.d;
    return result;
}

struct B B_orp(struct B* a, struct B* b) {
    struct B result;
    result.a = a->a | b->a;
    result.b = a->b | b->b;
    result.c = a->c | b->c;
    result.d = a->d | b->d;
    return result;
}

Real addr(Real a, Real b) { return a + b; }
Real subr(Real a, Real b) { return a - b; }
Real mulr(Real a, Real b) { return a * b; }
Real divr(Real a, Real b) { return a / b; }

LongReal addrl(LongReal a, LongReal b) { return a + b; }
LongReal subrl(LongReal a, LongReal b) { return a - b; }
LongReal mulrl(LongReal a, LongReal b) { return a * b; }
LongReal divrl(LongReal a, LongReal b) { return a / b; }

ExtendedReal addre(ExtendedReal a, ExtendedReal b) { return a + b; }
ExtendedReal subre(ExtendedReal a, ExtendedReal b) { return a - b; }
ExtendedReal mulre(ExtendedReal a, ExtendedReal b) { return a * b; }
ExtendedReal divre(ExtendedReal a, ExtendedReal b) { return a / b; }


bool eqol(LongOrdinal a, LongOrdinal b) { return a == b; }
bool neol(LongOrdinal a, LongOrdinal b) { return a != b; }
bool lol(LongOrdinal a, LongOrdinal b) { return a < b; }
bool leol(LongOrdinal a, LongOrdinal b) { return a <= b; }
bool gol(LongOrdinal a, LongOrdinal b) { return a > b; }
bool geol(LongOrdinal a, LongOrdinal b) { return a >= b; }

bool eqil(LongInteger a, LongInteger b) { return a == b; }
bool neil(LongInteger a, LongInteger b) { return a != b; }
bool lil(LongInteger a, LongInteger b) { return a < b; }
bool leil(LongInteger a, LongInteger b) { return a <= b; }
bool gil(LongInteger a, LongInteger b) { return a > b; }
bool geil(LongInteger a, LongInteger b) { return a >= b; }

bool eqo(Ordinal a, Ordinal b) { return a == b; }
bool neo(Ordinal a, Ordinal b) { return a != b; }
bool lo(Ordinal a, Ordinal b) { return a < b; }
bool leo(Ordinal a, Ordinal b) { return a <= b; }
bool go(Ordinal a, Ordinal b) { return a > b; }
bool geo(Ordinal a, Ordinal b) { return a >= b; }

bool eqi(Integer a, Integer b) { return a == b; }
bool nei(Integer a, Integer b) { return a != b; }
bool li(Integer a, Integer b) { return a < b; }
bool lei(Integer a, Integer b) { return a <= b; }
bool gi(Integer a, Integer b) { return a > b; }
bool gei(Integer a, Integer b) { return a >= b; }

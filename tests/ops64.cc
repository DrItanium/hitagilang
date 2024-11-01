#include <stdint.h>
typedef uint32_t Ordinal;
typedef int32_t Integer;
struct B {
    Ordinal _a;
    Ordinal _b;
    Ordinal _c;
    Ordinal _d;
    B(Ordinal a = 0, Ordinal b = 0, Ordinal c = 0, Ordinal d = 0) : _a(a), _b(b), _c(c), _d(d) { }
};

B operator&(const B& a, const B& b) { return B(a._a & b._a, a._b & b._b, a._c & b._c, a._d & b._d); }
B operator|(const B& a, const B& b) { return B(a._a | b._a, a._b | b._b, a._c | b._c, a._d | b._d); }
B operator^(const B& a, const B& b) { return B(a._a ^ b._a, a._b ^ b._b, a._c ^ b._c, a._d ^ b._d); }
B operator~(const B& a) { return B(~a._a, ~a._b, ~a._c, ~a._d); }



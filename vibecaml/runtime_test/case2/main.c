#include <stdint.h>
#include <stdlib.h>

void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

void verify_stack() {
    register void *sp __asm__("sp");

    if (((uintptr_t)sp & 0xF) != 0) {
        abort();
    }
}

long add(long x, long y) {
    verify_stack();
    return x + y;
}

long madd(long x, long y, long z) {
    verify_stack();
    return x * y + z;
}

void vc_main() {
    verify_stack();

    long args1[] = {10, -10};
    long args2[] = {-12};

    void* closure1 = vc_closure(&add, 2);
    void* result1 = vc_apply(closure1, 2, args1);

    vc_print_int((long) result1);
    vc_print_newline();

    void* closure2 = vc_closure(&madd, 3);
    void* closure3 = vc_apply(closure2, 1, args2);
    void* closure4 = vc_apply(closure3, 1, args2);
    void* result2 = vc_apply(closure4, 1, args2);

    vc_print_int((long) result2);
    vc_print_newline();

    void* closure5 = vc_closure(&madd, 3);
    void* closure6 = vc_apply(closure5, 2, args1);
    void* result3 = vc_apply(closure6, 1, args2);

    vc_print_int((long) result3);
    vc_print_newline();
}

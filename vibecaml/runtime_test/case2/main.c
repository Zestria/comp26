void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

long add(long x, long y) {
    return x + y;
}

long madd(long x, long y, long z) {
    return x * y + z;
}

void vc_main() {
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
}

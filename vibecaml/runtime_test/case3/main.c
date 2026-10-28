void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

long madd(long x, long y, long z) {
    return x * y + z;
}

void* get(long x) {
    if (x > 0) {
        long y = 7;
        return vc_apply(vc_closure(&madd, 3), 1, &y);
    } else {
        long y = 5;
        return vc_apply(vc_closure(&madd, 3), 1, &y);
    }
}

void vc_main() {
    long args1[] = {865, 2, 10};
    long args2[] = {-213, 4, 13};

    void* result1 = vc_apply(vc_closure(&get, 1), 3, args1);
    void* result2 = vc_apply(vc_closure(&get, 1), 3, args2);

    vc_print_int((long) result1);
    vc_print_newline();

    vc_print_int((long) result2);
    vc_print_newline();
}

void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

long factorial(long n) {
    if (n < 2) {
        return 1;
    }
    return n * factorial(n - 1);
}

void vc_main() {
    long n = 6;
    void* result = vc_apply(vc_closure(&factorial, 1), 1, &n);

    vc_print_int((long) result);
    vc_print_newline();

    n = 10;
    result = vc_apply(vc_closure(&factorial, 1), 1, &n);

    vc_print_int((long) result);
    vc_print_newline();

    n = 3;
    result = vc_apply(vc_closure(&factorial, 1), 1, &n);

    vc_print_int((long) result);
    vc_print_newline();
}

void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

#define BIG_NUMBER (10000000)

long add(long x, long y) {
    return x + y;
}

void vc_main() {
    for (unsigned long i = 0; i < BIG_NUMBER; ++i) {
        void* res = vc_closure(&add, 2);
    }
}

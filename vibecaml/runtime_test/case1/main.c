#include <limits.h>

void vc_fail(long error);
void vc_print_int(long number);
void vc_print_newline();

void* vc_closure(void* code, long arity);
void* vc_apply(void* closure, long argc, void* argv);

void vc_main() {
    vc_print_int(0);
    vc_print_newline();

    vc_print_int(-5432);
    vc_print_newline();

    vc_print_int(8641);
    vc_print_newline();

    vc_print_int(LONG_MAX);
    vc_print_newline();

    vc_print_int(LONG_MIN);
    vc_print_newline();

    vc_fail(0);
}

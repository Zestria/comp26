#ifndef MMML_RUNTIME_H
#define MMML_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* All generated code and C helpers use the RV64 C calling convention. */
typedef uint64_t mmml_value;
typedef mmml_value (*mmml_code)(const mmml_value *args);

#define MMML_FALSE UINT64_C(2)
#define MMML_TRUE UINT64_C(6)
#define MMML_UNIT UINT64_C(10)

/* The first implementation retains allocated closures until shutdown.
   Initialization is performed by startup.c before generated code runs. */
void mmml_init(void);
void mmml_shutdown(void);
_Noreturn void mmml_fail(const char *message);

mmml_value mmml_int(int64_t number);
int64_t mmml_unbox_int(mmml_value value);
mmml_value mmml_bool(bool value);
bool mmml_unbox_bool(mmml_value value);
bool mmml_is_closure(mmml_value value);

/* Prints the integer value (decimal, newline-terminated) and returns it. */
mmml_value mmml_print_int(mmml_value value);

/* Arity includes captured arguments. Each application supplies one argument. */
mmml_value mmml_make_closure(mmml_code code, size_t arity);
mmml_value mmml_apply(mmml_value function, mmml_value argument);

#endif

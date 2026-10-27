#ifndef BDSML_RUNTIME_H
#define BDSML_RUNTIME_H

#include <stdint.h>

/* Untagged 64-bit value; integers use signed two's-complement interpretation. */
typedef uint64_t bd_value;

/* Code address only: this C type does not describe the function's arity.
   Invoke through the assembly adapter using the agreed RV64 ABI, never
   through this pointer in C. No hidden closure argument is passed. */
typedef void (*bd_code_ptr)(void);

/* Closure allocations use malloc and live until process termination.
   No closure release API, reference counting, or garbage collector yet.
   Allocation failure or allocation-size overflow reports an error to stderr
   and terminates the process with exit status 1; it is not a bd_value result.
   Check size/count arithmetic before allocating or copying argument arrays.
   Temporary runtime buffers must be released after use. */

/* Create a closure with no captured arguments. Requires code != NULL and
   arity >= 1. The returned value represents a pointer to the closure. */
bd_value bd_make_closure(bd_code_ptr code, uint64_t arity);

/* Apply arguments in source order to a valid closure.
   argv must contain argc values and may be NULL when argc == 0.
   With zero new arguments, return the original closure unchanged.
   Underapplication returns a new closure without modifying the original;
   saturation invokes the code; overapplication applies remaining arguments
   to the returned closure (which must be a function in a well-typed program).
   argv is borrowed for this call only: copy any arguments retained by a new
   closure, and never retain the caller's array pointer. */
bd_value bd_apply(bd_value closure, uint64_t argc, const bd_value *argv);

/* Print the signed 64-bit interpretation of value as decimal to stdout,
   without a newline. Supports the full int64 range and returns unit (0).
   Also callable through a closure with arity 1 and no captured arguments. */
bd_value bd_print_int(bd_value value);

/* Print "division by zero\n" to stderr and terminate with exit status 1.
   Generated code checks the divisor before executing the RV64 div instruction. */
_Noreturn void bd_division_by_zero(void);

#endif /* BDSML_RUNTIME_H */

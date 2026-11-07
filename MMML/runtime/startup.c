#include "runtime.h"

/* The backend provides this symbol and evaluates top-level definitions in
   source order. Output is produced only by explicit language primitives. */
extern mmml_value mmml_program_main(void);

int main(int argc, char **argv) {
  (void)argv;
  /* &argc lives at the top of the outermost frame: the conservative
     collector scans the stack between its own frame and this address. */
  mmml_set_stack_base(&argc);
  mmml_init();
  (void)mmml_program_main();
  mmml_shutdown();
  return 0;
}

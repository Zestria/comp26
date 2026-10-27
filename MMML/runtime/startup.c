#include "runtime.h"

/* The backend provides this symbol and evaluates top-level definitions in
   source order. Output is produced only by explicit language primitives. */
extern mmml_value mmml_program_main(void);

int main(void) {
  mmml_init();
  (void)mmml_program_main();
  mmml_shutdown();
  return 0;
}

#include "runtime.h"

#include <string.h>

static mmml_value identity(const mmml_value *args) { return args[0]; }

int main(int argc, char **argv) {
  mmml_init();
  if (argc != 2) mmml_fail("expected one test name");
  if (strcmp(argv[1], "apply") == 0)
    (void)mmml_apply(mmml_int(1), MMML_UNIT);
  else if (strcmp(argv[1], "arity") == 0)
    (void)mmml_make_closure(identity, 0);
  else if (strcmp(argv[1], "integer") == 0)
    (void)mmml_unbox_int(MMML_TRUE);
  else if (strcmp(argv[1], "boolean") == 0)
    (void)mmml_unbox_bool(mmml_int(1));
  else if (strcmp(argv[1], "range") == 0)
    (void)mmml_int(INT64_C(1) << 62);
  else
    mmml_fail("unknown test name");
  return 0;
}

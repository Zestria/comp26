  $ cc -std=c11 -O2 -Wall -Wextra -Werror -pedantic runtime.c runtime_test.c -o runtime_test
  $ ./runtime_test
  MMML runtime: values, closures and CPS passed
  $ cc -std=c11 -O2 -Wall -Wextra -Werror -pedantic runtime.c startup.c startup_test.c -o startup_test
  $ ./startup_test
  7
  $ cc -std=c11 -O2 -Wall -Wextra -Werror -pedantic runtime.c runtime_error_test.c -o runtime_error_test
  $ ./runtime_error_test apply 2>&1
  MMML runtime: expected closure
  [1]
  $ ./runtime_error_test arity 2>&1
  MMML runtime: closure needs code and positive arity
  [1]
  $ ./runtime_error_test integer 2>&1
  MMML runtime: expected integer
  [1]
  $ ./runtime_error_test boolean 2>&1
  MMML runtime: expected boolean
  [1]
  $ ./runtime_error_test range 2>&1
  MMML runtime: integer outside 63-bit tagged range
  [1]

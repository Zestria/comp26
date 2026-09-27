#include <stdint.h>
extern "C" int32_t factorial(int32_t n) {
  if (n==0)
    return 1;
  else
    return n * factorial(n-1);
}
auto fac_ptr = factorial;
int main() {
    (void) factorial;
    return 0;
}
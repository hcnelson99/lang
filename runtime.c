#include <stdio.h>

extern int my_main(void);

int main(void) {
  int res = my_main();
  printf("%d\n", res);
  return res;
}

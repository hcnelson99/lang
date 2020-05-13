#include <stdio.h>

extern int my_main(void);

int main(void) {
  printf("%d\n", my_main());
  return 0;
}

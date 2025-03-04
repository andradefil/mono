/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * This algorithm walks a numeric argument
 * and prints the sum of the elements.
 */
#include <stdio.h>
// p input arg
char *p;
int n;
int main(int argc, char *argv[]) {
  p = argv[1];
  while (*p >= '0' && *p <= '9' && *p != '\0')
    // walks and sum
    n = n + *p++ - '0';
  printf("%d\n", n);
  return 0;
}

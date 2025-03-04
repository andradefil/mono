/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * This algorithm compares two integers A,B 
 * and prints the highest.
 */
#include <stdio.h>
int main() {
  int hi = ({ int a = 9; int b = 3; int z = 0;
    if (a > b) z = a;
    else z = b;
    z; });
    printf("%d\n", hi);
  return 0;
}

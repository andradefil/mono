/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * This algorithm loops over two axis x,y
 * and prints the multiplication of x,y;
 */
#include <stdio.h>
int main() {
  for (int x = 10, y = 1; x > y, y <= 10; x-=1, y++)
    printf ("%dX%d=%d\n", x, y, x*y);
  return 0;
}

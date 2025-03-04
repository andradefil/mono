/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * This algorithm prints differently for even rows.
 */
#include <stdio.h>
int main() {
  char m [4][3] = {
    {'*', '*', '*'},
    {'*', '*', '*'},
    {'*', '*', '*'},
    {'*', '*', '*'}
  };
  int row, col=0;
  for (row=0; row<4; row++) {
    for(col=0; col<3; col++)
      {printf("%c ", (row % 2 == 0) ? 'O' : m[row][col]);}
    printf("\n");
  }
  return 0;
}

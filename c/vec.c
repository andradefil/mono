/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * This algorithm prints 'Hello!'
 */
#include <stdio.h>

struct Message
{
    char v[6];
};

int main() {
  struct Message m = { {'H', 'e', 'l', 'l', 'o', '!'}};
  for (int i = 0; i < 6; i++)
    printf("%c", m.v[i]);
  printf("\n");
  return 0;
}

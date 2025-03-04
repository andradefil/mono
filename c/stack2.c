/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * Simple Stack Algorithm in C.
 * 
 */

#include <stdio.h>

#define MAX_ELEM 3
#define START_HEAD 0

int stack[MAX_ELEM];
int head;

void create();
void push(int elem, int *head);
int  pop(int *head);
int  size();
void printStack();

void create() {
  head = START_HEAD;
};

int size() {
  if (head > START_HEAD) {
    return head;
  } else {
    return START_HEAD;
  }
};

void printStack() {
  printf("%p,%d=>", &head, head);
  for (int i = head - 1; i >= 0; i--) {
    printf("%d,", stack[i]);
  };
  printf("\n");
  return;
};

void push(int elem, int *head) {
  if (*head < MAX_ELEM) {
    stack[*head] = elem;
    *head = *head + 1;
    printStack();
  } else {
    printf("Stack is full.\n");
  };
};

int pop(int *head) {
  int v = stack[*head];
  if (*head != START_HEAD) {
    *head = *head-1;
    printStack();
  } else {
    printf("Stack is empty.\n");
  };
  return v;
};

int main() {
  int op, s, v = 1;
  create();
  do {
    printf("1. push\n");
    printf("2. pop\n");
    printf("3. size\n");
    printf("4. print\n");
    printf("0. quit\n");

    scanf("%d",&op);
    switch(op) {
    case 1:
      scanf("%d",&v);
      push(v, &head);
      break;
    case 2:
      pop(&head);
      break;
    case 3:
      s = size();
      printf("size=>%d\n", s);
      break;
      break;
    case 4:
      printStack();
      break;
    default:
      break;
    };
  } while(op != 0);

  printf("Terminated.\n");
  return 0;
}

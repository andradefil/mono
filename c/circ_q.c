/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * Bounded Buffer Algorithm in C, using a circular queue.
 *
 * Each call to push get an ID, and enqueue an item,
 * limited to a max number of items in queue.
 * The FIFO order are respected to unqueue the items.
 * When number of items < MAX, more items can be enqueued.
 *
 */
#include <stdio.h>

#define MAX_ELEM 3
#define START_HEAD 0

int queue[MAX_ELEM];
int head;
int tail;
int sz;

void create();
void push(int *elem, int *pos, char err[]);
int  pop(int *head, char err[]);
int  size();
void printq();

void create() {
  head = START_HEAD;
  tail = START_HEAD;
  sz = 0;
};

void printq() {
  printf("%p,sz=%d,h=%d,t=%d=>", &head,sz,head,tail);
  int h = head;
  // iterate n number of elements
  // prints next item, starting from the head;
  for (int n = 0; n < sz; n++) {
    // ensure to read right position after reset buffer.
    if (h == MAX_ELEM) {
      h = START_HEAD;
    };
    printf("%d,", queue[h]);
    h++;
  };
  printf("\n");
  return;
};

void push(int *elem, int *tail, char err[]) {
  if (sz < MAX_ELEM) {
    if (*tail == MAX_ELEM) {
      *tail = START_HEAD;
    }

    queue[*tail] = *elem;
    *tail = *tail + 1;
    sz = sz + 1;
    *elem = *elem + 1;
    printq();
  } else {
    printf("%s\n", err);
  };
};

int pop(int *head, char err[]) {
  if (sz > START_HEAD) {
    int v;
    if (*head == MAX_ELEM) {
      *head = START_HEAD;
    }

    v= queue[*head];
    *head = *head + 1;
    sz = sz - 1;
    printq();
    return v;
  } else {
    printf("%s\n", err);
  };
  return -1;
};

int main() {
  int op, v = 1;
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
      // increment id if queued, prints err when queue is full.
      push(&v, &tail, "Queue is full.");
      break;
    case 2:
      // prints err when queue is empty.
      pop(&head, "Queue is empty.");
      break;
    case 3:
      printf("size=>%d\n", sz);
      break;
    case 4:
      printq();
      break;
    default:
      break;
    };
  } while(op != 0);

  printf("Terminated.\n");
  return 0;
}

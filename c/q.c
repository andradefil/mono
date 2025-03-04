/** Copyright (c) 2024 Filipe W. V. Andrade
 *
 * Bounded Queue in C, using a linked list.
 * Assigns a val for each item in the queue,
 * continue queuing/dequeuing as per the limit.
 *
 */
#include <stdio.h>
#include <stdlib.h>

#define MAX_ELEM 3
#define EMPTY_QUEUE 0

typedef struct item {
  int val;
  struct item *next;
} item_q;

item_q *head_q = NULL;
item_q *tail_q = NULL;
int sz;

void create();
void push(int *elem, item_q **head, item_q **tail, char err[]);
int  pop(item_q **head, char err[]);
int  size();
void printq();

void create() {
  sz = 0;
};

void printq() {
  item_q *current = head_q;
  printf("%p,sz=%d=>", &current,sz);
  while (current != NULL) {
    printf("%d,", current->val);
    current = current->next;
  }
  printf("\n");
  return;
};

void push(int *elem, item_q **head, item_q **tail, char err[]) {
  if (sz < MAX_ELEM) {
    item_q *new_item = malloc(sizeof(item_q));
    
    new_item->val = *elem;
    new_item->next = NULL;
   
    if (*head == NULL) {
      *head = new_item;
      *tail = *head;
    }
    else {
      item_q *current = *tail;
      current->next = new_item;
      *tail = new_item;
    };
    
    sz = sz + 1;
    *elem = *elem + 1;
    printq();
  } else {
    printf("%s\n", err);
  };
};

int pop(item_q **head, char err[]) {
  if (sz > EMPTY_QUEUE) {
    item_q *current = *head;
    *head = current->next;
    free(current);
   
    sz = sz - 1;
    printq();
    return current->val;
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
      // increment id, prints err when queue is full.
      push(&v, &head_q, &tail_q, "Queue is full.");
      break;
    case 2:
      pop(&head_q, "Queue is empty.");
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

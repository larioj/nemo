#nemo export append append
#nemo include (checkin List.c) List
#nemo include (checkin ListNode.c) ListNode
#nemo include (checkin size.c) listSize

#include "stdlib.h"

void *append(struct List *list, unsigned int allocationSize) {
  struct ListNode* node = malloc(sizeof *node);
  if (!allocationSize) {
    allocationSize = list->allocationSize;
  }
  node->value = malloc(allocationSize);
  node->next = NULL;
  if (listSize(list) == 0) {
    list->first = node;
  } else {
    list->last->next = node;
  }
  list->last = node;
  list->size++;
  return node->value;
}

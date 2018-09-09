#nemo export append
#nemo include List_dFpi3NequBIhWFFcHm4YGXn0A7osxM1Ip9GUBKHTPFEe List

#include "stdlib.h"

void **append(struct List **list) {
  struct List* node = malloc(sizeof *node);
  node->head = NULL;
  node->tail = NULL;
  if (!(*list)) {
    (*list) = node;
  } else {
    struct List *p = (*list);
    while (p->tail) {
      p = p->tail;
    }
    p->tail = node;
  }
  return &node->head;
}

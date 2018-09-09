#nemo export append
#nemo include List_745fa2dcd7aab8122158515c1e6e181979f403ba2cc4cd48fbd19404a1d33c51 List

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

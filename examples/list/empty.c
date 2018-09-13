#nemo export empty empty
#nemo include (checkin List.c) List

#include "stdlib.h"

struct List *empty(unsigned int allocationSize) {
  struct List *l = malloc(sizeof *l);
  l->size = 0;
  l->allocationSize = allocationSize;
  l->first = NULL;
  l->last = NULL;
  return l;
}

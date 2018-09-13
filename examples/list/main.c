#include "stdio.h"

#nemo include (checkin List.c) List
#nemo include (checkin append.c) append
#nemo include (checkin empty.c) empty
#nemo include (checkin size.c) listSize

int main() {
  struct List *list = empty(sizeof(int));
  for (int i = 0; i < 10; i++) {
    int *cur = append(list, 0);
    *cur = i;
  }
  printf("list size: %d\n", listSize(list));
  return 0;
}

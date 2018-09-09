#nemo export size
#nemo include List_dFpi3NequBIhWFFcHm4YGXn0A7osxM1Ip9GUBKHTPFEe List

int size(struct List *list) {
  int s = 0;
  while (list) {
    s++;
    list = list->tail;
  }
  return s;
}

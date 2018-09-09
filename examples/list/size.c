#nemo export size
#nemo include List_745fa2dcd7aab8122158515c1e6e181979f403ba2cc4cd48fbd19404a1d33c51 List

int size(struct List *list) {
  int s = 0;
  while (list) {
    s++;
    list = list->tail;
  }
  return s;
}

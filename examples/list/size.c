#nemo export size_fun size
#nemo include (copy List.c) List

int size_fun(struct List *list) {
  return list->size;
}

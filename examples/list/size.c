#nemo export size_fun size
#nemo include (checkin List.c) List

int size_fun(struct List *list) {
  return list->size;
}

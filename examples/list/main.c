#include "stdio.h"

#nemo include List_745fa2dcd7aab8122158515c1e6e181979f403ba2cc4cd48fbd19404a1d33c51 List
#nemo include size_bc000dc5a74862dd4380791d9c0ce0818945bf299e204719373683fb96f2909e size
#nemo include append_980a0feb25654cc6815ad0372626d3bfd0b78f1f19165f19191f79884dd20320 append

int main() {
  struct List *mylist = NULL;
  append(&mylist);
  append(&mylist);
  int mylist_size = size(mylist);
  printf("%d", mylist_size);
  return 0;
}

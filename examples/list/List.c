#nemo export List List
#nemo include (checkin ListNode.c) ListNode

struct List {
 unsigned int size;
 unsigned int allocationSize;
 struct ListNode *first;
 struct ListNode *last;
};

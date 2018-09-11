C List
======

Workspace
---------
  : vsplit
  : lcd $HOME/Projects/nemo/examples/list

Sources
-------
- NOTES.md
- ListNode.c
- List.c
- size.c
- empty.c
- append.c
- main.c

Commands
--------
  $ nemo checkin ListNode.c
  $ nemo checkin List.c
  $ git grep 'checkin size.c'

  $ alias nemo='stack exec nemo --'
    nemo cat main.c > /tmp/foo.c
    gcc /tmp/foo.c


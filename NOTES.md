Development Notes
=================

Vim Commands
------------
  : vsplit
  : lcd $HOME/Projects/nemo

Common Files
------------
- package.yaml
- stack.yaml
- src/Lib.hs
- app/Main.hs
- test/Spec.hs

Useful Commands
---------------
  $ stack ghci
  $ ghci
  $ stack build
  $ stack test
  $ stack exec nemo-exe
  $ git add .
  $ git commit -q
  $ git status | grep ':'
  $ git diff
  $ git stash
  $ git stash show -p
  $ stack install hfmt
  $ stack exec hfmt -- -w

Manual Testing
--------------
  $ cd examples/list
    stack build
    alias nemo='stack exec nemo-exe --'
    tree .
    nemo init
    tree .
    nemo checkin List.c
    nemo checkin append.c
    nemo checkin size.c
    tree .
    nemo cat main.c
    rm -rf nemolib

Example Project
---------------
- examples/list/List.c
- examples/list/append.c
- examples/list/main.c
- examples/list/size.c

Scratch
-------

  

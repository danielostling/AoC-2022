Common Lisp solutions to Advent of Code 2022

If you put these systems into a directory not in ~/quicklisp/local-projects, you can use the following to get quicklisp to pick it up.

In Emacs/Sly or Emacs/Slime (or in SBCL for that matter):
```
(pushnew (truename "/path/to/where/you/put/the/systems/day-1/") ql:*local-projects-directories*)
(ql:register-local-projects)
(ql:quickload "day-1")
(in-package :day-1)
(main)
```

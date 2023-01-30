;;;; day-9.lisp

(in-package #:day-9)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-rope-steps (input)
  "Read `input` and return list of (direction stepcount) pairs."
  (iter
    (for line in input)
    (for (dir-str count-str) = (uiop:split-string line))
    (collect (list (coerce dir-str 'character) (parse-integer count-str)))))


(defun test-step-sequence ()
  "Test if given step sequence generates visited position list given in puzzle
   test case."
  (let ((step-sequence '((#\R 4) (#\U 4) (#\L 3) (#\D 1)
                         (#\R 4) (#\D 1) (#\L 5) (#\R 2))))
    ;; Fill in code here..
    (test-visited-positions step-sequence)))

(defun position-list-equal-p (position-list1 position-list2)
  "Return T if `position-list1` and `position-list2` are equal, else NIL.
   `position-list1` and `position-list2` are lists of positions,
   ((x1 y1) (x2 y2) ... (xn yn)), where x1..n and y1..n are integers."
  (flet ((compare-positions (pos1 pos2)
            "Compare `pos1` to `pos2`.
             Return true if and only if the first argument is strictly less
             than the second (in some appropriate sense). If the first argument
             is greater than or equal to the second (in the appropriate sense),
             then return false."
           (cond ((< (first pos1) (first pos2)) t)
                 ((> (first pos1) (first pos2)) NIL)
                 (t (< (second pos1) (second pos2))))))
    (let ((sorted-positions1 (sort (copy-seq position-list1) #'compare-positions))
          (sorted-positions2 (sort (copy-seq position-list2) #'compare-positions)))
      (equal sorted-positions1 sorted-positions2))))


(defun test-visited-positions (visited-positions)
  "Test if `visited-positions` matches the test case in the puzzle.
   Starting position s is lower left corner, at (0, 0).

   ..##..   4
   ...##.   3
   .####.   2
   ....#.   1
   s###..   0

   012345

   Visited positions should then be (in (X Y)),
   Y0: (0 0) (1 0) (2 0) (3 0)
   Y1: (4 1)
   Y2: (1 2) (2 2) (3 2) (4 2)
   Y3: (3 3) (4 3)
   Y4: (2 4) (3 4)"
  (let ((test-case-positions '((0 0) (1 0) (2 0) (3 0)
                               (4 1) (1 2) (2 2) (3 2)
                               (4 2) (3 3) (4 3) (2 4)
                               (3 4))))
    (equal test-case-positions visited-positions)))


(defun solve-part-1 (parsed-steps)
  "Solve part 1."
  (declare (ignorable parsed-steps))
  "Part 1 TBD.")


(defun solve-part-2 (parsed-steps)
  "Solve part 2."
  (declare (ignorable parsed-steps))
  "Part 2 TBD.")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 9."
  (let* ((raw-rope-steps (get-input #P"./input"))
         (parsed-steps (parse-rope-steps raw-rope-steps))
         (part-1 (solve-part-1 parsed-steps))
         (part-2 (solve-part-2 parsed-steps)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

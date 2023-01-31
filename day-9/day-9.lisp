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


(defun calculate-tail-move (head-pos tail-pos)
  "Based on `head-pos` (x y) and `tail-pos` (i j), figure out where
   to move tail. If no change is needed, return `tail-pos`.
   This is a pretty naïve solution, I'm afraid but it gets the job done."
  (flet ((one-step-behind (i)
           "Return 1 if `i` is negative integer, else -1."
           (if (minusp i) 1 -1)))
    (let* ((head-x (first head-pos))
           (head-y (second head-pos))
           (tail-x (first tail-pos))
           (tail-y (second tail-pos))
           (distance-x (abs (- head-x tail-x)))
           (distance-y (abs (- head-y tail-y))))

 ;;; Cases
 ;;;   6 4 6
 ;;; 7 1 1 1 7
 ;;; 3 1 X 1 2
 ;;; 7 1 1 1 7
 ;;;   6 5 6
      (cond ((and (<= distance-x 1) (<= distance-y 1)) tail-pos) ;; case 1
            ((and (= head-y tail-y) (> head-x tail-x))           ;; case 2
             (list (1- head-x) tail-y)) 
            ((and (= head-y tail-y) (< head-x tail-x))           ;; case 3
             (list (1+ head-x) tail-y))
            ((and (= head-x tail-x) (> head-y tail-y))           ;; case 4
             (list tail-x (1- head-y)))
            ((and (= head-x tail-x) (< head-y tail-y))           ;; case 5
             (list tail-x (1+ head-y)))
            ((= distance-y 2)                                    ;; case 6
             (list head-x (+ head-y (one-step-behind (- head-y tail-y)))))
            ((= distance-x 2)                                    ;; case 7
             (list (+ head-x (one-step-behind (- head-x tail-x))) head-y))))))


(defun walk-steps (steps)
  "Go through the steps in the rope simulation. `steps` is a list of
  (direction stepcount) pairs, where direction is one of #\U #\L #\R #\D and
  stepcount is a positive integer. Collect all positions tail part of rope
  passes and return the (unique) list of (x y) positions, where x and y are
  integers. Start out with head and tail at same position, (0 0), which is in
  the lower left corner of a coordinate system."
  (let* ((head-pos '(0 0))
         (tail-pos '(0 0))
         (visited-positions '((0 0))))
    (iter
      (for (direction stepcount) in steps)
      (iter
        (for steps-taken from 1 to stepcount)
        (for (head-x head-y) = head-pos)
        (cond ((char-equal direction #\L) (setq head-pos (list (1- head-x) head-y)))
              ((char-equal direction #\U) (setq head-pos (list head-x (1+ head-y))))
              ((char-equal direction #\R) (setq head-pos (list (1+ head-x) head-y)))
              ((char-equal direction #\D) (setq head-pos (list head-x (1- head-y)))))
        (setq tail-pos (calculate-tail-move head-pos tail-pos))
        (pushnew tail-pos visited-positions :test #'equal)))
    visited-positions))


(defun compare-positions (pos1 pos2)
  "Compare `pos1` to `pos2`.
   Return true if and only if the first argument is strictly less
   than the second (in some appropriate sense). If the first argument
   is greater than or equal to the second (in the appropriate sense),
   then return false."
  (cond ((< (first pos1) (first pos2)) t)
        ((> (first pos1) (first pos2)) NIL)
        (t (< (second pos1) (second pos2)))))


(defun position-list-equal-p (position-list1 position-list2)
  "Return T if `position-list1` and `position-list2` are equal, else NIL.
   `position-list1` and `position-list2` are lists of positions,
   ((x1 y1) (x2 y2) ... (xn yn)), where x1..n and y1..n are integers."
  (let ((sorted-positions1 (sort (copy-seq position-list1) #'compare-positions))
        (sorted-positions2 (sort (copy-seq position-list2) #'compare-positions)))
    (equal sorted-positions1 sorted-positions2)))


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
    (position-list-equal-p test-case-positions visited-positions)))


(defun test-step-sequence ()
  "Test if given step sequence generates visited position list given in puzzle
   test case."
  (let ((step-sequence '((#\R 4) (#\U 4) (#\L 3) (#\D 1)
                          (#\R 4) (#\D 1) (#\L 5) (#\R 2))))
    (test-visited-positions (walk-steps step-sequence))))


(defun solve-part-1 (parsed-steps)
  "Solve part 1."
  (length (walk-steps parsed-steps)))


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

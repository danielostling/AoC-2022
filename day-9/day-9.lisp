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
   I'm afraid this is rather naïve solution, but it gets the job done.

   Head-Tail relation cases, where tail is initially at X in center, and head is
   at positions 1-7 or A-D.
   A 6 4 6 B
   7 1 1 1 7
   3 1 X 1 2
   7 1 1 1 7
   D 6 5 6 C"
  (flet ((one-step-behind (i)
           "Return 1 if `i` is negative integer, else -1."
           (if (minusp i) 1 -1)))
    (let* ((head-x (first head-pos))
           (head-y (second head-pos))
           (tail-x (first tail-pos))
           (tail-y (second tail-pos))
           (distance-x (abs (- head-x tail-x)))
           (distance-y (abs (- head-y tail-y))))
      (cond ((and (<= distance-x 1) (<= distance-y 1)) tail-pos) ;; Case 1
            ((and (= head-y tail-y) (> head-x tail-x))           ;; Case 2
             (list (1- head-x) tail-y)) 
            ((and (= head-y tail-y) (< head-x tail-x))           ;; Case 3
             (list (1+ head-x) tail-y))
            ((and (= head-x tail-x) (> head-y tail-y))           ;; Case 4
             (list tail-x (1- head-y)))
            ((and (= head-x tail-x) (< head-y tail-y))           ;; Case 5
             (list tail-x (1+ head-y)))
            ((and (= distance-x 2) (= distance-y 2))             ;; Move diag.
             (cond ((and (< head-x tail-x) (> head-y tail-y))
                    (list (1+ head-x) (1- head-y)))              ;; Case A
                   ((and (> head-x tail-x) (> head-y tail-y))
                    (list (1- head-x) (1- head-y)))              ;; Case B
                   ((and (> head-x tail-x) (< head-y tail-y))
                    (list (1- head-x) (1+ head-y)))              ;; Case C
                   ((and (< head-x tail-x) (< head-y tail-y))
                    (list (1+ head-x) (1+ head-y)))))            ;; Case D
            ((= distance-y 2)                                   ;; Case 6
             (list head-x (+ head-y (one-step-behind (- head-y tail-y)))))
            ((= distance-x 2)                                   ;; Case 7
             (list (+ head-x (one-step-behind (- head-x tail-x))) head-y))))))


(defun walk-steps (steps &optional (knots 2))
  "Go through the steps in the rope simulation.

  `steps` is a list of (direction stepcount) pairs, where direction is one of
  #\U #\L #\R #\D and stepcount is a positive integer.

  `knots` is an integer denoting how many positions to keep track of. Two knots
  is one head and one tail. The knots will move using the previous knot as local
  head.

  Collect all positions tail knot of the rope passes and return the (unique)
  list of the tail knot (x y) positions, where x and y are integers. Start out
  with all knots at the same position, (0 0) in this simulation."
  (let ((knot-positions (iter
                          (for knot from 1 to knots)
                          (collect '(0 0))))
        (visited-positions '((0 0))))
    (iter
      (for (direction stepcount) in steps)
      (for move-line first 1 then (1+ move-line))
      (iter
        (for steps-taken from 1 to stepcount)
        (for (head-x head-y) = (first knot-positions))
        (setf (nth 0 knot-positions)
              (cond ((char-equal direction #\L) (list (1- head-x) head-y))
                    ((char-equal direction #\U) (list head-x (1+ head-y)))
                    ((char-equal direction #\R) (list (1+ head-x) head-y))
                    ((char-equal direction #\D) (list head-x (1- head-y)))))
        (iter
          (for local-tail-index from 1 below (length knot-positions))
          (for local-head-index first 0 then (1+ local-head-index))
          (for local-head = (nth local-head-index knot-positions))
          (for local-tail = (nth local-tail-index knot-positions))
          (setf (nth local-tail-index knot-positions)
                (calculate-tail-move local-head local-tail)))
        (pushnew (car (last knot-positions)) visited-positions :test #'equal)))
    visited-positions))


(defun solve-part-1 (parsed-steps)
  "Solve part 1."
  (length (walk-steps parsed-steps)))


(defun solve-part-2 (parsed-steps)
  "Solve part 2."
  (length (walk-steps parsed-steps 10)))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 9."
  (let* ((raw-rope-steps (get-input #P"./input"))
         (parsed-steps (parse-rope-steps raw-rope-steps))
         (part-1 (solve-part-1 parsed-steps))
         (part-2 (solve-part-2 parsed-steps)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

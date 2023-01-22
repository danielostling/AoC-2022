;;;; day-5.lisp

(in-package #:day-5)

(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun strip-char-from-list (lst char)
  "Remove char from list."
  (remove-if #'(lambda (el) (equal el char)) lst))


(defun empty-string-p (str)
  "True if string is of 0 length, else false."
  (= (length str) 0))


(defun transpose-list-of-lists (lists)
  "'Transpose' a list of lists.
   This turns     into
   ((1 0 0)      ((1 1 0)
    (1 0 0)  ==>  (0 0 0)
    (0 0 1))      (0 0 1))"
  (apply #'mapcar #'list lists))


(defun make-crate-stacks (input)
  "Given stack specification, build and return a list of lists, representing
   crate stacks.

   Input:
           [M]     [B]             [N]
   [T]     [H]     [V] [Q]         [H]
   [Q]     [N]     [H] [W] [T]     [Q]
   [V]     [P] [F] [Q] [P] [C]     [R]
   [C]     [D] [T] [N] [N] [L] [S] [J]
   [D] [V] [W] [R] [M] [G] [R] [N] [D]
   [S] [F] [Q] [Q] [F] [F] [F] [Z] [S]
   [N] [M] [F] [D] [R] [C] [W] [T] [M]
    1   2   3   4   5   6   7   8   9 

   should return
   ((T Q V C D S N)
    (V F M)
    (M H N P D W Q F)
    (F T R Q D)
    (B V H Q N M F R)
    (Q W P N G F C)
    (T C L R F W)
    (S N Z T)
    (N H Q R J D S M))"

  (labels ((read-raw-crate-stacks (input)
           "Get the portion of the input which describes the crate stacks."
           (iter
             (for line in input)
             (when (string= line " 1   2   3   4   5   6   7   8   9 ")
               (return crate-portion))
             (collect line into crate-portion)))
           (horizontal-stacks (raw-stacks &key (crate-string-width 4))
             "Extract crate letters and blanks."
             (iter
               (for row in raw-stacks)
               (for row-as-list = (coerce row 'list))
               (for row-chars = (iter 
                                  (for idx
                                       from 1
                                       below (length row-as-list)
                                       by crate-string-width)
                                  (for char = (nth idx row-as-list))
                                  (collect char)))
               (collect row-chars)))
           (add-dummy-crate (crate-stack)
             "Add a dummy crate at bottom of each stack. This simplifies stack
             manipulation as it ensures no stack is ever empty."
             (append crate-stack (list #\%))))
    (let* ((raw-crate-stack-strings (read-raw-crate-stacks input))
           (crate-string-width 4)
           (crate-stack-lists
             (mapcar
              (lambda (s) (coerce s 'list))
              raw-crate-stack-strings))
           (just-crates-and-blanks
             (horizontal-stacks
              crate-stack-lists
              :crate-string-width crate-string-width))
           (crate-stacks (mapcar
                          #'(lambda (stack-with-blanks)
                              (strip-char-from-list stack-with-blanks #\ ))
                          (transpose-list-of-lists just-crates-and-blanks))))
      (mapcar #'add-dummy-crate crate-stacks))))


(defun get-crate-moves (input)
  "Return crate move sequences"
  (flet ((make-move (raw-move-string)
           "Convert 'move 1 from 8 to 7' into (1 8 7)."
           (let* ((raw-move-str-list
                    (uiop:split-string
                     (remove-if #'alpha-char-p raw-move-string)))
                  (move-str-list (remove-if #'empty-string-p raw-move-str-list)))
             (mapcar #'parse-integer move-str-list))))
    (let ((raw-move-strings
            (iter
              (for line in input)
              (if (and
                   (>= (length line) 5)
                   (string= (subseq line 0 5) "move "))
                  (collect line)
                  (next-iteration)))))
      (mapcar #'make-move raw-move-strings))))


(defun move-crates-part-1 (crate-stacks moves)
  "Move crates between crate stacks, one crate at a time, according to
   instructions in input, and return final crate-stacks."
  (iter
    (for (crates-to-move source-stack target-stack) in moves)
    (iter
      (for crates-moved from 1 to crates-to-move)
      (push
       (pop
        (nth (1- source-stack)
             crate-stacks))
       (nth (1- target-stack) crate-stacks))))
  crate-stacks)


(defun move-crates-part-2 (crate-stacks moves)
  "Move crates between crate stacks, multiple crates at a time, according to
   instructions in input, and return final crate-stacks."
  (iter
    (for (crates-to-move source-stack target-stack) in moves)
    (for crates-picked-up =
         (reverse
          (iter
            (for crates-moved from 1 to crates-to-move)
            (collect (pop (nth (1- source-stack) crate-stacks))))))
    (iter
      (for crate-to-put-down in crates-picked-up)
      (push crate-to-put-down (nth (1- target-stack) crate-stacks))))
  crate-stacks)


(defun build-crate-message (crate-stacks)
  "Construct and return a message from the first elements of each crate stack."
  (let* ((first-char-each-stack (mapcar #'first crate-stacks)))
    (coerce first-char-each-stack 'string)))


(defun solve-part-1 (input)
  "Solve part 1."
  (let* ((crate-stacks (make-crate-stacks input))
         (crate-moves (get-crate-moves input))
         (final-crate-stacks (move-crates-part-1 crate-stacks crate-moves)))
    (build-crate-message final-crate-stacks)))


(defun solve-part-2 (input)
  "Solve part 2."
  (let* ((crate-stacks (make-crate-stacks input))
         (crate-moves (get-crate-moves input))
         (final-crate-stacks (move-crates-part-2 crate-stacks crate-moves)))
    (build-crate-message final-crate-stacks)))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 5."
  (let* ((raw-section-assignments (get-input #P"./input"))
         (part-1 (solve-part-1 raw-section-assignments))
         (part-2 (solve-part-2 raw-section-assignments)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

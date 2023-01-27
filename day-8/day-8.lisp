;;;; day-8.lisp

(in-package #:day-8)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun list-of-strings-to-array (strings)
  "Convert list of strings to array."
  (let* ((width (length (first strings)))
         (height (length strings))
         (numbers (reverse
                   (iter
                     (for s in strings)
                     (for numbers = (mapcar #'digit-char-p (coerce s 'list)))
                     (collect numbers))))
         (arr (make-array (list width height) :initial-contents numbers)))
    arr))


(defun get-row-from-array (row-idx arr)
  "Return row as list at `row-idx` in `arr`, zero-indexed."
  (let ((width (array-dimension arr 0)))
    (iter
      (for idx from 0 below width)
      (collect (aref arr row-idx idx)))))


(defun get-col-from-array (col-idx arr)
  "Return col as list at `col-idx` in `arr`, zero-indexed."
  (let ((height (array-dimension arr 1)))
    (iter
      (for idx from 0 below height)
      (collect (aref arr idx col-idx)))))


(defun generate-tree-positions (arr)
  "Generate list of (x y) pairs denoting each position to examine."
  (let* ((arr-width )
         (arr-height)))

  
  )



(defun solve-part-1 (input)
  "Solve part 1."
  (declare (ignorable input))
  "Part 1 TBD.")


(defun solve-part-2 (input)
  "Solve part 2."
  (declare (ignorable input))
  "Part 2 TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 8."
  (let* ((raw-tree-map (get-input #P"./input"))
         (part-1 (solve-part-1 raw-tree-map))
         (part-2 (solve-part-2 raw-tree-map)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

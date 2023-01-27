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


(defun delete-nth (seq n)
  "Delete element at index `n` from sequence `seq`.
   From https://groups.google.com/g/comp.lang.lisp/c/JesrFuUT7ak/m/3l24nU5mPYsJ"
  (delete-if (constantly t) (copy-seq seq) :start n :count 1))


(defun partition (seq n)
  "Split `seq` at index `n` and return ((left-elements) (right-elements))."
  (let ((left (subseq seq 0 n))
        (right (subseq seq (1+ n))))
    (list left right)))


(defun tree-visible-p (row-idx col-idx arr)
  "Test if tree at (`row-idx` `col-idx`) in `arr` is visible from edges."
  (let* ((tree-value (aref arr row-idx col-idx))
         (row (get-row-from-array row-idx arr))
         (col (get-col-from-array col-idx arr)))
    (if (or
         (member row-idx (list 0 (1- (length row))))
         (member col-idx (list 0 (1- (length col)))))
        t
        (progn
          (destructuring-bind (row-left row-right) (partition row col-idx)
            (destructuring-bind (col-left col-right) (partition col row-idx)
              (or
               (> tree-value (apply #'max row-left))
               (> tree-value (apply #'max row-right))
               (> tree-value (apply #'max col-left))
               (> tree-value (apply #'max col-right)))))))))


(defun get-visible-trees (arr)
  "Figure out how many trees that are visible from edges."
  (let* ((arr-width (array-dimension arr 0))
         (arr-height (array-dimension arr 1))
         (visible-trees (iter outer
                          (for col from 0 below arr-width)
                          (iter
                            (for row from 0 below arr-height)
                            (in outer
                                (counting (tree-visible-p row col arr)))))))
    visible-trees))


(defun scenic-score (row col arr)
  "Calculate scenic score at position (`row` `col`) in `arr`."
  (let* ((tree-value (aref arr row-idx col-idx))
         (row (get-row-from-array row-idx arr))
         (col (get-col-from-array col-idx arr)))
    (destructuring-bind (row-left row-right) (partition row col-idx)
      (destructuring-bind (col-left col-right) (partition col row-idx)
        (let ((looking-left (reverse row-left))
              (looking-right row-right)
              (looking-up (reverse col-left))
              (looking-down col-right))
          


          )))
    )  
  )


(defun get-scenic-score (arr)
  "Figure out maximum 'scenic score' defined by how many trees that are visible
   in each direction multiplied together."
  (let* ((arr-width (array-dimension arr 0))
         (arr-height (array-dimension arr 1))
         (max-scenic-score (iter outer
                             (for col from 1 below (1- arr-width))
                             (iter
                               (for row from 1 below (1- arr-height))
                               (in outer
                                   (maximizing (scenic-score row col arr)))))))
    max-scenic-score))


(defun solve-part-1 (arr)
  "Solve part 1."
  (get-visible-trees arr))


(defun solve-part-2 (arr)
  "Solve part 2."
  (declare (ignorable arr))
  "Part 2 TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 8."
  (let* ((raw-tree-map (get-input #P"./input"))
         (tree-arr (list-of-strings-to-array raw-tree-map))
         (part-1 (solve-part-1 tree-arr))
         (part-2 (solve-part-2 tree-arr)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

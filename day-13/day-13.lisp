;;;; day-13.lisp

(in-package #:day-13)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun assoc-v (item alist &key (default 0))
  "Get just the value, not the key also."
  (let ((pair (assoc item alist :test #'equal)))
    (cond ((null pair) default)
          (t (cdr pair)))))


(defun parse-packet-string (packet-string)
  "Parse a packet string into a (possibly nested) list."
  (let* ((packet-parsed (copy-seq packet-string))
         (replacements '((#\[ #\()
                         (#\] #\))
                         (#\, #\Space))))
    (iter
      (for (old new) in replacements)
      (setf packet-parsed (substitute new old packet-parsed :test #'equal)))
    (read-from-string packet-parsed)))


(defun parse-packet-data (raw-packet-data)
  "Parse `raw-packet-data` and return data structure."
  (let ((input (copy-seq raw-packet-data))
        (pairs nil)
        (pair nil)
        (part nil))
    (iter
      (when (null input)
        (when pair
          (push (reverse pair) pairs))
        (leave pairs))
      (setf part (pop input))
      (when (= (length part) 0)
        (push (reverse pair) pairs)
        (setf pair nil)
        (next-iteration))
      (push (parse-packet-string part) pair))
    (reverse pairs)))


(defun compare-pair (left right &optional (results nil))
  "Compare packets `left` to `right` using puzzle rules. If  `left` is
   determined to be 'bigger', return T else NIL.

   Rules:
   1) If both values are integers, the lower integer should come first. If the
      left integer is lower than the right integer, the inputs are in the right
      order. If the left integer is higher than the right integer, the inputs
      are not in the right order. Otherwise, the inputs are the same integer;
      continue checking the next part of the input.
   2) If both values are lists, compare the first value of each list, then the
      second value, and so on. If the left list runs out of items first, the
      inputs are in the right order. If the right list runs out of items first,
      the inputs are not in the right order. If the lists are the same length
      and no comparison makes a decision about the order, continue checking the
      next part of the input.
   3) If exactly one value is an integer, convert the integer to a list which
      contains that integer as its only value, then retry the comparison. For
      example, if comparing [0,0,0] and 2, convert the right value to [2] (a
      list containing 2); the result is then found by instead comparing [0,0,0]
      and [2]."
  (flet ((compare-int (a b)
           "a and b are integers. Return
            :in-order if a < b,
            :out-of-order if a > b,
            :undecided if a = b"
           (cond ((< a b) :in-order)
                 ((> a b) :out-of-order)
                 ((= a b) :undecided))))
    ; (format t "Comparing ~a to ~a, results: ~a~%" left right results)
    (cond ((and (integerp left) (integerp right))
           (compare-int left right))
          ((and (listp left) (listp right))
           (cond ((and (null left) (null right)) (append results '(:undecided)))
                 ((null left) (append results '(:in-order)))
                 ((null right) (append results '(:out-of-order)))
                 (t (compare-pair
                     (rest left)
                     (rest right)
                     (append results (list (compare-pair (first left) (first right))))))))
          ((integerp left) (compare-pair (list left) right results))
          ((integerp right) (compare-pair left (list right) results)))))


(defun extract-result (result-list)
  "Get result from compare-pair call.
   Figure out if :in-order symbol or :out-of-order symbol comes first."
  (cond ((null result-list) :undecided)
        ((equal :in-order (first result-list)) :in-order)
        ((equal :out-of-order (first result-list)) :out-of-order)
        (t (extract-result (rest result-list)))))


(defun unpack-packet-pairs (packet-pairs)
  "Unpack list of pairs of packets `packet-pairs` into a list of packets."
  (iter
    (for (a b) in packet-pairs)
    (collect a)
    (collect b)))


(defun sort-predicate (packet-1 packet-2)
  "Interpreting function between stable-sort and compare-pair."
  (let ((result (extract-result
                 (alexandria:flatten
                  (compare-pair packet-1 packet-2)))))
    (if (equal result :in-order) t nil)))


(defun solve-part-1 (packet-data)
  "Solve part 1."
  (iter
    (for (packet-1 packet-2) in packet-data)
    (for index initially 1 then (1+ index))
    (for result = (extract-result
                   (alexandria:flatten
                    (compare-pair packet-1 packet-2))))
    (when (equal :in-order result)
      (summing index))))


(defun solve-part-2 (packet-data)
  "Solve part 2."
  (let* ((packets-unsorted (unpack-packet-pairs packet-data))
         (divider-packets '(((2)) ((6))))
         (packets-with-dividers (append packets-unsorted divider-packets))
         (sorted-packets (stable-sort packets-with-dividers #'sort-predicate)))
    (iter
      (for packet in sorted-packets)
      (for index initially 1 then (1+ index))
      (when (member packet divider-packets :test #'equal)
        (multiplying index)))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 13."
  (let* ((raw-packet-data (get-input #P"./input"))
         (packet-data (parse-packet-data raw-packet-data)))
    (let ((part-1 (solve-part-1 packet-data))
          (part-2 (solve-part-2 packet-data)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

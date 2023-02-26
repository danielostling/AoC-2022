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


(defun compare-pair (left-list right-list &optional (counter 1))
  "Compare `left-list` to `right-list` using puzzle rules. If  `left-list` is
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
  (format t "Comparing~%~a~%to~%~a~%-------------~%" left-list right-list)
  (let ((left (copy-seq left-list))
        (right (copy-seq right-list)))
    (format t "left list null: ~a~%" (null left))
    (format t "right list null: ~a~%" (null right))
    (cond ((> counter 150)   ; recursion limiter
           (progn
             (format t "Recursion limit hit => NIL~%")
             nil)) 
          ((and (null left-list) right-list)   ; Rule 2
           (progn
             (format t "left null, right != null => T~%")
             t))
          ((and (null right-list) left-list)   ; Rule 2
           (progn
             (format t "left != null, right null => NIL~%")
             nil))
          ((and (null left-list) (null right-list))
           (progn
             (format t "ran out of lists without hitting NIL condition => T~%")
             t))
          ((and (integerp (first left))   ; Rule 1
                (integerp (first right))
                (> (first left) (first right)))
           (progn
             (format t "left int > right int => nil~%")
             nil)) 
          (t (let ((left-element (pop left))
                   (right-element (pop right)))
                (format t "Comparing left: ~a with right: ~a~%"
                        left-element
                        right-element)
                (format t "After pop, left is now: ~a and right is now: ~a~%" left right)
               ;; Rule 3; if one element is an integer and the other is a list,
               ;;         wrap the integer in a list and compare again.
               (if (and (listp left-element) (listp right-element))
                   (progn
                      (format t "list elements ~a and ~a are lists, "
                              left-element right-element)
                      (format t "recursing into them~%")
                     (compare-pair left-element right-element (1+ counter)))
               (progn
                     (cond ((and (integerp left-element)
                                 (not (integerp right-element)))
                            (progn
                               (format t "converting left ~a to list~%"
                                       left-element)
                              (push (list left-element) left)
                              (push right-element right)))
                           ((and (not (integerp left-element))
                                 (integerp right-element))
                            (progn
                               (format t "converting right ~a to list~%"
                                       right-element)
                              (push left-element left)
                              (push (list right-element) right)))
                           (t (progn
                                 (format t "left ~a and right ~a are integers "
                                         left-element right-element)
                                 (format t "and left <= right => T~%")
                                )
                                )
                           )
                     (compare-pair left right (1+ counter))
                     )    
                   )
               ))
          )
    ))


(defun test-cases ()
  "Run through example test cases to see if solution might work."
  (let* ((test-cases '((:case-1 (1 1 3 1 1)   (1 1 5 1 1)   t)
                       (:case-2 ((1) (2 3 4)) ((1) 4)       t)
                       (:case-3 (9)           ((8 7 6))     nil)
                       (:case-4 ((4 4) 4 4)   ((4 4) 4 4 4) t)
                       (:case-5 (7 7 7 7)     (7 7 7)       nil)
                       (:case-6 ()            (3)           t)
                       (:case-7 ((()))        (())          nil)
                       (:case-8 (1 (2 (3 (4 (5 6 7)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 9) nil)
                       (:case-9 (((7 (6 9) 0 2) 6 (3 7)) (9 5)) () nil)
                       (:case-10 ((9)) (() (9) ((10 (6 7 8 7 9)) (0 0 (9 10 0)))) nil)
                       ))
         (run-tests '(:case-10))
         ;; (run-tests '(:case-1 :case-2 :case-3 :case-4 :case-5 :case-6 :case-7 :case-8))
         (results
           (iter
             (for (test-case arg1 arg2 expected-result) in test-cases)
             (when (not (or (member test-case run-tests) (member :all run-tests)))
               (next-iteration))
             (for actual-result = (funcall #'compare-pair arg1 arg2))
             (for is-expected = (equal actual-result expected-result))
             (collect (format nil "~a: Should return ~a, actual ~a => Testcase is ~a~%"
                              test-case expected-result actual-result is-expected)))))
    (dolist (result results)
      (format t result))))



;(1:(((4) (2) 5 (3 5 9) 6) 6)              2:(7 (6 4)) 3:(((8 4 0 2 8)) 0 4) 4:(((9 2) (5 10 1))))
;(1:(5 ((7 5) 0 (3 10 4) (8 0)) (1 3 2 1)) 2:(5 7)     3:(((1 3 5 9 3) 10 (8 9) 0 10) 6 1 5))




(defun solve-part-1 (packet-data)
  "Solve part 1."
  (iter
    (for (packet-1 packet-2) in packet-data)
    (for index initially 1 then (1+ index))
    (when (compare-pair packet-1 packet-2)
      (format t "Index ~a~%" index)
      (summing index))))


(defun solve-part-2 (packet-data)
  "Solve part 2."
  (declare (ignorable packet-data))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 12."
  (let* ((raw-packet-data (get-input #P"./input"))
         (packet-data (parse-packet-data raw-packet-data)))
    (let ((part-1 (solve-part-1 packet-data))
          (part-2 (solve-part-2 packet-data)))
      (format t "Packet data length: ~a~%" (length packet-data))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

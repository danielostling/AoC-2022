;;;; day-3.lisp

(in-package #:day-3)


(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun make-rucksack-compartments (item-string)
  "Split item-string in half and convert to two lists of characters."
  (let* ((item-string-length (length item-string))
         (compartment-string-length (/ item-string-length 2))
         (first-compartment (coerce
                             (subseq
                              item-string
                              0
                              compartment-string-length)
                             'list))
         (second-compartment (coerce
                              (subseq
                               item-string
                               compartment-string-length
                               item-string-length)
                              'list)))
    (values first-compartment second-compartment)))


(defun find-duplicate-items (list1 list2)
  "Return list of items which are only in one of the lists.
   Why both intersect and remove-duplicates? Function intersect
   will return duplicates in some cases, for example
   (intersection '(#\h #\h #\h) '(#\a #\b #\c #\h))"
  (remove-duplicates (intersection list1 list2) :test #'equal))


(defun item-to-priority (item)
  "Given an item, return its integer priority value.

  - Lowercase item types a through z have priorities 1 through 26.
  - Uppercase item types A through Z have priorities 27 through 52.

  ASCII A => 65
  ASCII Z => 90
  ASCII a => 97
  ASCII z => 122"

  (let* ((character-code (char-code item))
         (in-range-1-p (and (> character-code 64) (< character-code 91)))
         (in-range-2-p (and (> character-code 96) (< character-code 123))))
    (cond (in-range-1-p (+ 26 (- character-code 64)))  ;; A -> Z
          (in-range-2-p (- character-code 96)))))      ;; a -> z


(defun items-to-priorities (items)
  "Convert items (characters) to respective priority integer."
  (mapcar #'item-to-priority items))


(defun rucksack-string-to-priority (rucksack-string)
  "Given a rucksack item string, return its priority value."
  (multiple-value-bind (compartment1-items compartment2-items)
      (make-rucksack-compartments rucksack-string)
    (reduce #'+ (items-to-priorities (find-duplicate-items
                                      compartment1-items
                                      compartment2-items)))))


(defun count-by-hash (h elem)
  "Destructively add item elem to hash h and increase it's count (the value) by one."
  (multiple-value-bind (val present) (gethash elem h)
    (cond
      ((null present) (setf (gethash elem h) 1))
      (t (setf (gethash elem h) (1+ val))))))


(defun find-common-items (item-strings)
  "Return items shared by all item strings in given list."
  (let* ((n-item-strings (length item-strings))
         (item-count (make-hash-table)))
    (iter
      (for item-string in item-strings)
      (for item-codes = (remove-duplicates (coerce item-string 'list) :test #'equal))
      (iter
        (for item-code in item-codes)
        (count-by-hash item-count item-code)))
    (iter
      (for (item-code count) in-hashtable item-count)
      (when (= count n-item-strings)
        (collect item-code)))))


(defun item-type-by-group (rucksack-item-strings)
  "Find item type common to each group."
  (iter
    (for (elf1 elf2 elf3) on rucksack-item-strings by #'cdddr)
    (for common-items = (find-common-items (list elf1 elf2 elf3)))
    (appending common-items)))


(defun solve-part-1 (input)
  "Solve part 1."
  (iter
    (for rucksack-string in input)
    (summing (rucksack-string-to-priority rucksack-string))))


(defun solve-part-2 (input)
  "Solve part 2."
  (reduce #'+ (items-to-priorities (item-type-by-group input))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 3."
  (let* ((rucksack-strings (get-input #P"./input"))
         (part-1 (solve-part-1 rucksack-strings))
         (part-2 (solve-part-2 rucksack-strings)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

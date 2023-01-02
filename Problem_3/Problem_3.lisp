;;;; Problem_3.lisp

(in-package #:Problem_3)

(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))

(defun rucksack-sets (item-string)
  "Split item-string in half and convert to two sets of characters."
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


(defun find-duplicate-items (set1 set2)
  "Return list of items which are only in one of the sets."
  (append
   (set-difference set1 set2 :test #'string=)
   (set-difference set2 set1 :test #'string=)))


(defun item-to-value (item)
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
  (mapcar #'item-to-value items))

(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 3."
  ;; (let* ((cals-per-elf (calories-per-elf (get-input #P"./input"))))
  ;;   (format t "First part: ~a~%" (first cals-per-elf))
  ;;   (format t "Second part: ~a~%" (reduce #'+ (first-n-elems cals-per-elf 3))))
  )

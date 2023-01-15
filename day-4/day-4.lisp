;;;; day-4.lisp

(in-package #:day-4)

(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun split-section-assignments (raw-section-string)
  "Given a section assignment string, like 31-57,30-58
   return list of corresponding integers
   (elf1-low elf1-high elf2-low elf2-high). In example above
   return (31 57 30 58)."

  (let ((parts (uiop:split-string raw-section-string :separator '(#\- #\,))))
    (mapcar #'parse-integer parts)))


(defun contained-p (ranges)
  "Given two value ranges encoded in list of four integers
   (min1 max1 min2 max2), return T if either range is contained in the other
   range or NIL if not.
  
   (0 3 4 7) => NIL
   (4 8 0 12) => T (first range contained in second range)"

  (flet ((range-contained-p (min1 max1 min2 max2)
           "Return T if integer range min2 -> max2 is contained within integer
            range min1 -> max1, otherwise return NIL."
           (and (>= min2 min1)
                (>= max1 max2))))
  (destructuring-bind
      (range1-min range1-max)
      (sort (subseq ranges 0 2) #'<)
    (destructuring-bind
        (range2-min range2-max)
        (sort (subseq ranges 2 4) #'<)
      (or (range-contained-p
           range1-min range1-max
           range2-min range2-max)
          (range-contained-p
           range2-min range2-max
           range1-min range1-max))))))


(defun overlaps-p (ranges)
  "Given two value ranges encoded in list of four integers
   (min1 max1 min2 max2), return T if either range overlaps the other range or
   NIL if not.
  
   (0 3 4 7) => NIL
   (4 8 0 12) => T (first range contained in second range)

   (2 4 6 8) => NIL, no overlap
   (2 3 4 5) => NIL, no overlap
   (5 7 7 9) => T, overlaps in a single section, 7.
   (2 8 3 7) => T, overlaps all of the sections 3 through 7.
   (6 6 4 6) => T, overlaps in a single section, 6.
   (2 6 4 8) => T, overlaps in sections 4, 5, and 6.

   Case 1
         min1....max1
    min2.....max2

    Case 2
         min1....max1
             min2......max2

    Edge cases
    - min and/or max values are same
    - one range wholly contains the other range."

  (flet ((range-overlaps-p (min1 max1 min2 max2)
           "Return T if integer range min2-max2 overlaps integer range
            min1-max1, otherwise return NIL."
           (or (and (>= min1 min2)
                    (>= max2 min1))
               (and (>= max1 min2)
                    (>= max2 max1)))))
         
  (destructuring-bind
      (range1-min range1-max)
      (sort (subseq ranges 0 2) #'<)
    (destructuring-bind
        (range2-min range2-max)
        (sort (subseq ranges 2 4) #'<)
      (or (range-overlaps-p
           range1-min range1-max
           range2-min range2-max)
          (range-overlaps-p
           range2-min range2-max
           range1-min range1-max))))))


(defun solve-part-1 (input)
  "Solve part 1."
  (iter
    (for raw-section-string in input)
    (counting (contained-p (split-section-assignments raw-section-string)))))


(defun solve-part-2 (input)
  "Solve part 2."
  (iter
    (for raw-section-string in input)
    (counting (overlaps-p (split-section-assignments raw-section-string)))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 4."
  (let* ((raw-section-assignments (get-input #P"./input"))
         (part-1 (solve-part-1 raw-section-assignments))
         (part-2 (solve-part-2 raw-section-assignments)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2))
  )

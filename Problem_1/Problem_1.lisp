;;;; Problem_1.lisp

(in-package #:Problem_1)

(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun calories-per-elf (input)
  "Sum calories by elf into list of integers. New elf when blank input is found."
  (let* ((cals-per-elf (iter
                         (with calorie-sum = 0)
                         (for calorie-str in input)
                         (if (string= calorie-str "")
                             (progn
                               (collect calorie-sum into cal-list)
                               (setq calorie-sum 0))
                             (setq calorie-sum
                                   (+ calorie-sum (parse-integer calorie-str))))
                         (finally (return cal-list)))))
    (sort (copy-seq cals-per-elf) #'>)))


(defun first-n-elems (lst count)
  "Return first count elements of lst."
  (subseq lst 0 count))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 1."
  (let* ((cals-per-elf (calories-per-elf (get-input #P"./input"))))
    (format t "First part: ~a~%" (first cals-per-elf))
    (format t "Second part: ~a~%" (reduce #'+ (first-n-elems cals-per-elf 3)))))

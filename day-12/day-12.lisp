;;;; day-12.lisp

(in-package #:day-12)


(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))

(defun parse-topology (raw-topology)
  "Parse `raw-topology`."
  (declare (ignorable raw-topology))
  "TBD")


(defun solve-part-1 (topology)
  "Solve part 1."
  (declare (ignorable topology))
  "TBD")


(defun solve-part-2 (topology)
  "Solve part 2."
  (declare (ignorable topology))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 12."
  (let* ((raw-topology (get-input #P"./input"))
         (topology (parse-topology raw-topology))
         (part-1 (solve-part-1 topology))
         (part-2 (solve-part-1 topology)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

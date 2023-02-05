;;;; day-10.lisp

(in-package #:day-10)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-instructions (raw-instructions)
  "Convert `raw-instructions` into a pair of integers (cycles register-adjust)
   where cycles is how many cycles the instruction takes and
   register-adjust is how much register X should be changed by instruction.

   Two instructions exists
   noop   -> (1 0)
   addx V -> (2 V)
   V may be negative."
  (flet ((split-instruction (raw-instruction)
           "Split instruction."
           (destructuring-bind
               (instruction &optional (register-adjust "0"))
               (uiop:split-string raw-instruction)
             (cond ((string= instruction "noop")
                    (list 1 0))
                   ((string= instruction "addx")
                    (list 2 (parse-integer register-adjust)))))))
    (iter
      (for raw-instruction in raw-instructions)
      (collect (split-instruction raw-instruction)))))


(defun run-program (instructions)
  "Run program consisting of `instructions`.
   Return list of X register integer value per cycle, where first value is
   cycle 1. Not happy with this implementation, should be done entirely in iter."
  (let* ((register-x-value 1)
         (register-x-per-clock
           (iter outer
             (for (cycle-cost register-adjust) in instructions)
             (iter
               (for cycles from 0 below cycle-cost)
               (in outer
                   (collect register-x-value)))
             (setq register-x-value (+ register-x-value register-adjust)))))
    (append register-x-per-clock (list register-x-value))))


(defun compare-pairs (pair1 pair2)
  "Compare `pair1` to `pair2`.
   Return true if and only if the first argument is strictly less than the
   second (in some appropriate sense). If the first argument is greater than or
   equal to the second (in the appropriate sense), then return false."
  (cond ((< (first pair1) (first pair2)) t)
        ((> (first pair1) (first pair2)) NIL)
        (t (< (second pair1) (second pair2)))))


(defun pair-list-equal-p (pairs1 pairs2)
  "Return T if `pairs1` and `pairs2` are equal, else NIL. `pairs1` and `pairs2`
   are lists of lists of integers, ((a1 b1) (a2 b2) ... (an bn))."
  (let ((sorted-pairs1 (sort (copy-seq pairs1) #'compare-pairs))
        (sorted-pairs2 (sort (copy-seq pairs2) #'compare-pairs)))
    (equal sorted-pairs1 sorted-pairs2)))


(defun test-program ()
  "Test program from day-10, first part."
  (let* ((instructions '("noop"
                         "addx 3"
                         "addx -5"))
         (expected-result '(1 1 1 4 4 -1))
         (actual-result (run-program (parse-instructions instructions))))
    (format t (concatenate 'string
               "Expected: ~a~%"
               "  Actual: ~a~%"
               "  Result: ~a~%")
            expected-result
            actual-result
            (equal (sort (copy-seq expected-result) #'>)
                   (sort (copy-seq actual-result) #'>)))))


(defun solve-part-1 (instructions)
  "Solve part 1."
  (let ((register-value-by-clock (run-program instructions)))
    (iter
      (for cycle in '(20 60 100 140 180 220))
      (sum (* cycle (nth (1- cycle) register-value-by-clock))))))


(defun solve-part-2 (instructions)
  "Solve part 2."
  (declare (ignorable instructions))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 10."
  (let* ((raw-instructions (get-input #P"./input"))
         (instructions (parse-instructions raw-instructions))
         (part-1 (solve-part-1 instructions))
         (part-2 (solve-part-2 instructions)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

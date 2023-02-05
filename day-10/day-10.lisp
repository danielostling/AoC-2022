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
   cycle 1. Not happy with this implementation, as the final value is tacked on
   outside the iteration. The whole thing should have been done in the iteration
   itself instead."
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


(defun build-image (instructions)
  "Use the register value and CRT beam position to build an image as a list of
   strings. See day 10 part 2 for problem description for now."
  (flet ((pixel-overlap-p (crt-x pixel-center)
           "Determine if pixel covers CRT beam X position. Return T if it does,
            else NIL."
           (let ((pixel-xs (list (1- pixel-center) pixel-center (1+ pixel-center))))
             (member crt-x pixel-xs :test #'=))))
    (let ((pixel-center-per-clock (run-program instructions))
          (line-width 40)
          (line-count 6))
      (iter 
        (for line-idx from 0 below line-count)
        (for line = (iter
                      (for crt-x from 0 below line-width)
                      (for beam-idx = (+ crt-x (* line-idx line-width)))
                      (for pixel-center = (nth beam-idx pixel-center-per-clock))
                      (if (pixel-overlap-p crt-x pixel-center)
                          (collect "#")
                          (collect "."))))
        (collect (format nil "~{~a ~}" line))))))


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


(defun test-program2 ()
  "Test program from day-10, second part."
  (let* ((instructions '("addx 15" "addx -11" "addx 6" "addx -3" "addx 5"
                         "addx -1" "addx -8" "addx 13" "addx 4" "noop" "addx -1"
                         "addx 5" "addx -1" "addx 5" "addx -1" "addx 5"
                         "addx -1" "addx 5" "addx -1" "addx -35" "addx 1"
                         "addx 24" "addx -19" "addx 1" "addx 16" "addx -11"
                         "noop" "noop" "addx 21" "addx -15" "noop" "noop"
                         "addx -3" "addx 9" "addx 1" "addx -3" "addx 8" "addx 1"
                         "addx 5" "noop" "noop" "noop" "noop" "noop" "addx -36"
                         "noop" "addx 1" "addx 7" "noop" "noop" "noop" "addx 2"
                         "addx 6" "noop" "noop" "noop" "noop" "noop" "addx 1"
                         "noop" "noop" "addx 7" "addx 1" "noop" "addx -13"
                         "addx 13" "addx 7" "noop" "addx 1" "addx -33" "noop"
                         "noop" "noop" "addx 2" "noop" "noop" "noop" "addx 8"
                         "noop" "addx -1" "addx 2" "addx 1" "noop" "addx 17"
                         "addx -9" "addx 1" "addx 1" "addx -3" "addx 11" "noop"
                         "noop" "addx 1" "noop" "addx 1" "noop" "noop"
                         "addx -13" "addx -19" "addx 1" "addx 3" "addx 26"
                         "addx -30" "addx 12" "addx -1" "addx 3" "addx 1" "noop"
                         "noop" "noop" "addx -9" "addx 18" "addx 1" "addx 2"
                         "noop" "noop" "addx 9" "noop" "noop" "noop" "addx -1"
                         "addx 2" "addx -37" "addx 1" "addx 3" "noop" "addx 15"
                         "addx -21" "addx 22" "addx -6" "addx 1" "noop" "addx 2"
                         "addx 1" "noop" "addx -10" "noop" "noop" "addx 20"
                         "addx 1" "addx 2" "addx 2" "addx -6" "addx -11" "noop"
                         "noop" "noop"))
         (expected-result '("##..##..##..##..##..##..##..##..##..##.."
                            "###...###...###...###...###...###...###."
                            "####....####....####....####....####...."
                            "#####.....#####.....#####.....#####....."
                            "######......######......######......####"
                            "#######.......#######.......#######....."))
         (actual-result (build-image  (parse-instructions instructions))))
    (format t (concatenate 'string
                           "Expected: ~%~a~%"
                           "----------~%"
                           "  Actual: ~%~a~%")
            expected-result
            actual-result)))



(defun solve-part-1 (instructions)
  "Solve part 1."
  (let ((register-value-by-clock (run-program instructions)))
    (iter
      (for cycle in '(20 60 100 140 180 220))
      (sum (* cycle (nth (1- cycle) register-value-by-clock))))))


(defun solve-part-2 (instructions)
  "Solve part 2."
  (build-image instructions))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 10."
  (let* ((raw-instructions (get-input #P"./input"))
         (instructions (parse-instructions raw-instructions))
         (part-1 (solve-part-1 instructions))
         (part-2 (solve-part-2 instructions)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~%~a~%" part-2)))

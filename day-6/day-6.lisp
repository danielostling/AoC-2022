;;;; day-6.lisp

(in-package #:day-6)

(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun find-packet-delimiter (datastream-buffer &optional (marker-size 4))
  "Find the first position where the marker-size most recently received
   characters were all different.

   Examples, marker-size = 4:
   0        1         2         3
   123456789012345678901234567890
   mjqjpqmgbljsphdztnvjfqwrcgsmlb
         ^ <- answer is 7
      .... <- marker

   0        1         2
   1234567890123456789012345678
   bvwbjplbgvbhsrlpgdmjqwftvncz
       ^ <- answer is 5

   0        1         2
   1234567890123456789012345678
   nppdvjthqldpwncqszvftbrmjlhg
        ^ <- answer is 6

   0        1         2         3
   123456789012345678901234567890123
   nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
            ^ <- answer is 10

   0        1         2         3
   12345678901234567890123456789012
   zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
             ^ <- answer is 11"

  (let ((datastream (coerce (first datastream-buffer) 'list))
        (marker-candidate '()))
    (iter
      (for char in datastream)
      (for idx first 1 then (1+ idx))
      (push char marker-candidate)
      (when (> (length marker-candidate) marker-size)
        (setf marker-candidate (butlast marker-candidate)))
      (when (and
             (= (length marker-candidate) marker-size)
             (= (length
                 (remove-duplicates (copy-seq marker-candidate) :test #'equal))
                marker-size))
        (leave (values idx marker-candidate))))))


(defun solve-part-1 (input)
  "Solve part 1."
  (find-packet-delimiter input 4))


(defun solve-part-2 (input)
  "Solve part 2."
  (find-packet-delimiter input 14))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 6."
  (let ((datastream-buffer (get-input #P"./input")))
    (multiple-value-bind (idx marker)
        (solve-part-1 datastream-buffer)
      (format t "First part: marker ~a at index ~a~%" marker idx))
             
    (multiple-value-bind (idx marker)
        (solve-part-2 datastream-buffer)
      (format t "Second part: marker ~a at index ~a~%" marker idx))))

;;;; day-15.lisp

(in-package #:day-15)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-sensors-and-beacons (raw-input-data)
  "Parse sensor and beacon coordinate and distance information from input data.
   Return a list of lists; (sensor-x sensor-y beacon-x beacon-y), all integers.
   Turns line
     Sensor at x=2, y=18: closest beacon is at x=-2, y=15
   into
     (2 18 -2 15)
   as one element of result list."

  (iter
    (for line in raw-input-data)
    (for raw-parts = (rest (uiop:split-string line :separator '(#\= #\, #\:))))
    (for sensor-x = (parse-integer (first raw-parts)))
    (for sensor-y = (parse-integer (third raw-parts)))
    (for beacon-x = (parse-integer (fifth raw-parts)))
    (for beacon-y = (parse-integer (seventh raw-parts)))
    (collect (list sensor-x sensor-y beacon-x beacon-y))))


(defun manhattan-distance (pos1 pos2)
  "Calculate manhattan distance between (x y) pairs `pos1` `pos2`.
   Distance is |x1 - x2| + |y1 - y2|."
  (+
   (abs
    (- (first pos1) (first pos2)))
   (abs
    (- (second pos1) (second pos2)))))


(defun make-map ()
  "Simulate sparse 2D map using a hash table.
   Use equal as test, as keys are '(x y) integers."
  (make-hash-table :test #'equal))


(defun initialize-map (map-hash sensors-and-beacons)
  "Populate `map-hash` with sensor and beacon coordinates from
  `sensors-and-beacons`. Modifies `map-hash` destructively."
  (iter
    (for (sensor-x sensor-y beacon-x beacon-y) in sensors-and-beacons)
    (setf (gethash `(,sensor-x ,sensor-y) map-hash) #\S)
    (setf (gethash `(,beacon-x ,beacon-y) map-hash) #\B)))


(defun map-dimensions (map-hash)
  "Return (min-x min-y max-x max-y map-hash)."
  (iter
    (for (key nil) in-hashtable map-hash)
    (for (x y) = key)
    (minimizing x into min-x)
    (minimizing y into min-y)
    (maximizing x into max-x)
    (maximizing y into max-y)
    (finally
     (return (list min-x min-y max-x max-y map-hash)))))


(defun mark (map-hash sensor beacon)
  "Mark sensor coverage between `sensor` and `beacon` in `map-hash`.
   `sensor` and `beacon` are (x y) integer positions in `map-hash`.
   Updates `map-hash.`. NOTE: This is a brute force approach to test if a
   position is in range or not."
  (let ((distance (manhattan-distance sensor beacon))
        (sensor-x (first sensor))
        (sensor-y (second sensor)))
    (iter
      (for y from (- sensor-y distance) to (+ sensor-y distance))
      (iter
        (for x from (- sensor-x distance) to (+ sensor-x distance))
        (when (and (<= (manhattan-distance sensor `(,x ,y)) distance)
                   (equal (gethash `(,x ,y) map-hash #\.) #\.))
          (setf (gethash `(,x ,y) map-hash) #\#))))))


(defun print-map (map-data)
  "Draw the map using `map-data` which is a list
   (min-x min-y max-x may-y map-hash) where
   min-{x,y} and max {x,y} are coordinate limits for the grid, and
   map-hash is a map hash table."
  (destructuring-bind (min-x min-y max-x max-y map-hash) map-data
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (for value = (gethash `(,x ,y) map-hash #\.))
        (format t "~a" value))
      (format t "~%"))))


(defun marks-per-row (map-hash row)
  "Return mark count in `map-hash` at `row`."
  (iter
    (for (key val) in-hashtable map-hash)
    (for (nil y) = key)
    (when (= y row)
      (counting (equal val #\#) into marks))
    (finally (return marks))))


(defun solve-part-1 (sensors-and-beacons row)
  "Solve part 1."
  (let ((map-hash (make-map)))
    (initialize-map map-hash sensors-and-beacons)
    (iter
      (for (sensor-x sensor-y beacon-x beacon-y) in sensors-and-beacons)
      (mark map-hash `(,sensor-x ,sensor-y) `(,beacon-x ,beacon-y)))
                                        ; (print-map (map-dimensions map-hash))
    (let ((marks (marks-per-row map-hash row)))
      (format t "Row ~a has ~a marks.~%" row marks)
      marks)))


(defun solve-part-2 (sensors-and-beacons)
  "Solve part 2."
  (declare (ignorable sensors-and-beacons))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 15."
  (let* ((raw-input-data (get-input #P"./input"))
         (sensors-and-beacons (parse-sensors-and-beacons raw-input-data)))
    (let ((part-1 (solve-part-1 sensors-and-beacons 2000000))
          (part-2 (solve-part-2 sensors-and-beacons)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

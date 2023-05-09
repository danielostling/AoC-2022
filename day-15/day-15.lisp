;;;; day-15.lisp

(in-package #:day-15)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-sensors-and-beacons (raw-input-data)
  "Parse sensor and beacon coordinate and distance information from input data.
   Return a list of lists; (sensor-x sensor-y beacon-x beacon-y distance),
   all integers.
   Turns line
     Sensor at x=2, y=18: closest beacon is at x=-2, y=15
   into
     (2 18 -2 15 7)
   as one element of result list."
  (iter
    (for line in raw-input-data)
    (for raw-parts = (rest (uiop:split-string line :separator '(#\= #\, #\:))))
    (for sensor-x = (parse-integer (first raw-parts)))
    (for sensor-y = (parse-integer (third raw-parts)))
    (for beacon-x = (parse-integer (fifth raw-parts)))
    (for beacon-y = (parse-integer (seventh raw-parts)))
    (collect
        (list sensor-x sensor-y beacon-x beacon-y
              (manhattan-distance
               (list sensor-x sensor-y)
               (list beacon-x beacon-y))))))


(defun manhattan-distance (pos1 pos2)
  "Calculate manhattan distance between (x y) pairs `pos1` `pos2`.
   Distance is |x1 - x2| + |y1 - y2|."
  (+
   (abs
    (- (first pos1) (first pos2)))
   (abs
    (- (second pos1) (second pos2)))))


(defun grid-dimensions (sensors-beacons-distances)
  "Calculate grid dimensions by locating outermost sensors and beacons in
   `sensors-beacons-distances`. Additionally, consider max sensor sweep area
   reaching outside edge beacons. Return (min-x min-y max-x max-y), all integers."
  (let ((min-x 0)
        (min-y 0)
        (max-x 0)
        (max-y 0))
    (iter
      (for (sensor-x sensor-y beacon-x beacon-y distance) in sensors-beacons-distances)
      (for (sweep-min-x sweep-min-y sweep-max-x sweep-max-y) = (square-sweep-dims (list sensor-x sensor-y) distance))
      (minimizing (min sensor-x beacon-x sweep-min-x) into grid-min-x)
      (minimizing (min sensor-y beacon-y sweep-min-y) into grid-min-y)
      (maximizing (max sensor-x beacon-x sweep-max-x) into grid-max-x)
      (maximizing (max sensor-y beacon-y sweep-max-y) into grid-max-y)
      (finally
       (progn
         (setf min-x grid-min-x)
         (setf min-y grid-min-y)
         (setf max-x grid-max-x)
         (setf max-y grid-max-y))))
    (list min-x min-y max-x max-y)))


(defun has-beacon-or-sensor (point sensors-beacons-distances)
  "Return B if point is a beacon, S if point is a sensor, else NIL."
  (let ((point-x (first point))
        (point-y (second point)))
    (iter
      (for (sensor-x sensor-y beacon-x beacon-y nil) in sensors-beacons-distances)
      (when (and (= point-x sensor-x) (= point-y sensor-y))
        (leave #\S))
      (when (and (= point-x beacon-x) (= point-y beacon-y))
        (leave #\B)))))


(defun circle-line-intersection (y sensor distance)
  "Given line, described by a horizontal line at `y` and a circle described by
   integer (x y) coordinates in `sensor` and its manhattan distance radius
   `distance`, return one of three cases;
   1) Line does not intersect circle => nil,
   2) Line touches circle edge => (touching-x-coordinate touching-x-coordinate),
   or
   3) Line intersects circle at two points => (left-x right-x) (line is horizontal)

   However. This problem deals with 'circles' described by manhattan distance,
   simplifying the problem of determining intersection points."

  (destructuring-bind (sensor-x sensor-y) sensor
    (let ((sensor-to-line (abs (- sensor-y y))))
      (cond
        ((> sensor-to-line distance) nil)  ; Case 1
        ((= sensor-to-line distance) (list sensor-x sensor-x))  ; Case 2
        (t  ; Case 3
         (let* ((remaining-horizontal-distance (- distance sensor-to-line))
                (left-x (- sensor-x remaining-horizontal-distance))
                (right-x (+ sensor-x remaining-horizontal-distance)))
           (list left-x right-x)))))))


(defun smallest-first (s1 s2)
  "Sort segments `s1` and `s2` so that smaller first x comes first.
   If first x are identical, smallest second x comes first."
  (let ((left-s1 (first s1))
        (right-s1 (second s1))
        (left-s2 (first s2))
        (right-s2 (second s2)))
    (cond
      ((= left-s1 left-s2) (< right-s1 right-s2))
      (t (< left-s1 left-s2)))))


(defun row-coverage-segments (row sensors-beacons-distances)
  "Given `row` and sensors, beacons and distances in `sensors-beacons-distances`,
  return a list of sensor covered parts of the row as a list of lists, where
  each list element is an integer pair (left-x right-x) describing the covered
  segment. If a sensor sweep just touches the row, left-x and right-y are
  identical. If no sensors reach the row, return nil."
  (let ((segments
          (iter
            (for (sensor-x sensor-y nil nil distance) in sensors-beacons-distances)
            (for coverage-segment = (circle-line-intersection row (list sensor-x sensor-y) distance))
            (when coverage-segment
              (collect coverage-segment)))))
    (sort segments #'smallest-first)))


(defun count-sensor-coverage (min-x max-x row sensors-beacons-distances)
  "Considering a `row` from `min-x` to `max-x`, count sensor coverage."
  (let ((count 0)
        (segments (row-coverage-segments row sensors-beacons-distances)))
    (iter
      (for x from min-x to max-x)
      (iter
        (for (left-x right-x) in segments)
        (when (and
               (>= x left-x)
               (<= x right-x)
               (not (has-beacon-or-sensor (list x row) sensors-beacons-distances)))
          (incf count)
          (leave))))
    count))


(defun find-gap (row min-x max-x sensors-beacons-distances)
  "Return integer (x y) position of point with no sensor coverage or NIL if
   `row` has no such position."
  (let ((segments (row-coverage-segments row sensors-beacons-distances))
        (x-pos min-x)
        (segment-found nil))
    (iter
      (when (>= x-pos max-x)
        ;(format t "X-pos ~a is >= max-x ~a, row done~%" x-pos max-x)
        (leave nil))
      (setf segment-found nil)
      (iter
        (for (left-x right-x) in segments)
        (when (and
               (>= x-pos left-x)
               (< x-pos right-x))
          (setf x-pos right-x)
          (setf segment-found t)))
      (when (null segment-found)
        (format t "Found segment gap in row ~a at x-pos ~a~%" row x-pos)
        (leave (list x-pos row)))
      (incf x-pos))))


(defun solve-part-1 (sensors-beacons-distances row)
  "Solve part 1."
  (let* ((grid-dims (grid-dimensions sensors-beacons-distances))
         (grid-min-x (first grid-dims))
         (grid-max-x (third grid-dims)))
    (count-sensor-coverage grid-min-x grid-max-x row sensors-beacons-distances)))


(defun solve-part-2 (sensors-beacons-distances min-x min-y max-x max-y)
  "Solve part 2."
  (let* ((distress-beacon-point
         (iter
           (for row from min-y to max-y)
           (for distress-beacon = (find-gap row min-x max-x sensors-beacons-distances))
           (when distress-beacon
             (leave distress-beacon))))
         (beacon-x (first distress-beacon-point))
         (beacon-y (second distress-beacon-point)))
    (+ (* beacon-x 4000000) beacon-y)))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 15."
  (let* ((raw-input-data (get-input #P"./input"))
         (sensors-beacons-distances (parse-sensors-and-beacons raw-input-data)))
    (let ((part-1 (solve-part-1 sensors-beacons-distances 2000000))
          (part-2 (solve-part-2 sensors-beacons-distances 0 0 4000000 4000000)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

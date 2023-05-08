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


(defun square-sweep-dims (sensor distance)
  "Given a `sensor` position, integer (x y) and an integer `distance` to
   nearest beacon, return the -+distance square centered at `sensor` in
   integer (min-x min-y max-x max-y) format."
  (let* ((sensor-x (first sensor))
         (sensor-y (second sensor))
         (square-min-x (- sensor-x distance))
         (square-min-y (- sensor-y distance))
         (square-max-x (+ sensor-x distance))
         (square-max-y (+ sensor-y distance)))
  (list square-min-x square-min-y square-max-x square-max-y)))


(defun quick-covered-point (point sensor distance)
  "Do a square shaped quick-test to rule out sensors 'far' from point.
   `point` is an integer (x y) coordinate to test,
   `sensor` is an integer (x y) sensor coordinate to compare to, and
   `distance` is the manhattan distance between `sensor` and its closest beacon.
   Return T if point is inside the test square, or NIL otherwise."
  (let* ((point-x (first point))
         (point-y (second point)))
    (destructuring-bind
        (square-min-x square-min-y square-max-x square-max-y)
        (square-sweep-dims sensor distance)
      (and
       (>= point-x square-min-x)
       (>= point-y square-min-y)
       (<= point-x square-max-x)
       (<= point-y square-max-y)))))


(defun covered-point (point sensors-beacons-distances)
  "Test if integer pair (x y) `point` is inside any of the sensor sweep areas
   described by `sensors-beacons-distances`, a list of integer lists
   (sensor-x sensor-y beacon-x beacon-y distance). Distance is manhattan
   distance between sensor and beacon. Return T if inside, otherwise NIL."
  (iter
    (for (sensor-x sensor-y nil nil distance) in sensors-beacons-distances)
    (when (quick-covered-point point (list sensor-x sensor-y) distance)
      (for distance-to-point = (manhattan-distance
                                point (list sensor-x sensor-y)))
      (when (>= distance distance-to-point)
        (leave t)))))


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


(defun calculate-distance (sensors-and-beacons)
  "Given a list of integer lists (sensor-x sensor-y beacon-x beacon-y)
   described by `sensors-and-beacons`, add the integer manhattan distance as
   last element in each sensor and beacon coordinate list and return it."
  (iter
    (for (sensor-x sensor-y beacon-x beacon-y) in sensors-and-beacons)
    (collect (list
              sensor-x sensor-y
              beacon-x beacon-y
              (manhattan-distance
               (list sensor-x sensor-y)
               (list beacon-x beacon-y))))))


(defun sensor-coverage (row sensors-beacons-distances)
  "Return how many coordinates there are in `row` which has sensor coverage."
  (let* ((covered 0)
         (dimensions (grid-dimensions sensors-beacons-distances))
         (min-x (first dimensions))
         (max-x (third dimensions)))
    (iter
      (for x from min-x to max-x)
      (for beacon-sensor = (has-beacon-or-sensor
                            (list x row) sensors-beacons-distances))
      (when beacon-sensor
        (format t "Beacon or sensor at ~a, ~a~%" x row)
        (next-iteration))
      (when (covered-point (list x row) sensors-beacons-distances)
          (incf covered)))
    covered))


(defun rescale-sensors-beacons-distances (factor sensors-beacons-distances)
  "Rescale sensors and beacons in `sensors-beacons-distances `by `factor`,
   then recalculate distance, and return rescaled data structure."
  (iter
    (for (sensor-x sensor-y beacon-x beacon-y nil) in sensors-beacons-distances)
    (for rescaled-sensor-x = (round (/ sensor-x factor)))
    (for rescaled-sensor-y = (round (/ sensor-y factor)))
    (for rescaled-beacon-x = (round (/ beacon-x factor)))
    (for rescaled-beacon-y = (round (/ beacon-y factor)))
    (for rescaled-sensor = (list rescaled-sensor-x rescaled-sensor-y))
    (for rescaled-beacon = (list rescaled-beacon-x rescaled-beacon-y))
    (collect (list rescaled-sensor-x rescaled-sensor-y
                   rescaled-beacon-x rescaled-beacon-y
                   (manhattan-distance
                    rescaled-sensor
                    rescaled-beacon)))))


(defun draw-map (sensors-beacons-distances &optional (scale 10000))
  "Draw the map!"
  (let ((rescaled-sensors-beacons (rescale-sensors-beacons-distances
                                   scale
                                   sensors-beacons-distances)))
    (destructuring-bind (min-x min-y max-x max-y) (grid-dimensions rescaled-sensors-beacons)
      (format t "Dims; at scale 1:~a, cols ~a -> ~a, rows ~a -> ~a~%"
              scale min-x max-x min-y max-y)
      (iter
        (for y from min-y to max-y)
        (iter
          (for x from min-x to max-x)
          (for s-or-b = (has-beacon-or-sensor (list x y) rescaled-sensors-beacons))
          (if s-or-b
              (format t "~a " s-or-b)
              (format t ". ")))
        (format t "~%")))))


(defun solve-part-1 (sensors-beacons-distances row)
  "Solve part 1."
  (sensor-coverage row sensors-beacons-distances))


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

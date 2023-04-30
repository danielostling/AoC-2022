;;;; day-14.lisp

(in-package #:day-14)

(defconstant +sand-source+ '(500 0)
  "(col row) of sand source.")


(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-rock-structures (raw-input-data)
  "Parse `raw-input-data` into a 2D array of rock structures.
  Each line in `raw-input-data` is a line segment set of one or more lines.

  x,y -> x,y -> ... where x is horizontal distance to the right and y is
  distance down, putting origo (0, 0) at top left corner."
  (labels ((remove-chars (seq chars)
             "Remove all occurences of each character in `chars` from `seq`."
             (if (null chars)
                 seq
                 (remove-chars (remove (first chars) seq) (rest chars))))
           (make-segment-corners (string-segment)
             "Split string segment into pairs of (x y) integer values."
             (let* ((trimmed-string-segment (remove-chars string-segment '(#\Space #\>)))
                    (string-pairs (uiop:split-string trimmed-string-segment :separator '(#\-))))
               (iter
                 (for string-pair in string-pairs)
                 (for int-pair = (mapcar #'parse-integer (uiop:split-string string-pair :separator '(#\,))))
                 (collect int-pair)))))
    (iter
      (for string-segment in raw-input-data)
      (collect (make-segment-corners string-segment)))))


(defun get-grid-dimensions (rock-structures)
  "Determine max width and height of the rock structures.
   Return integers (row cols)"
  (iter outer
    (for segment in rock-structures)
    (iter
      (for (right-distance down-distance) in segment)
      (in outer
          (maximizing right-distance into max-cols)
          (maximizing down-distance into max-rows)))
    (finally
     (return-from outer (list max-cols max-rows)))))


(defun make-coordinate-range (start stop)
  "Return list of all coordinate pairs between `start` and `stop`."
  (let* ((pair-diff (list
                     (- (first start) (first stop))
                     (- (second start) (second stop))))
         (step-col  (cond ((zerop (first pair-diff)) 0)
                          ((plusp (first pair-diff)) -1)
                          (t 1)))
         (step-row  (cond ((zerop (second pair-diff)) 0)
                          ((plusp (second pair-diff)) -1)
                          (t 1))))
    (iter
      (when (equal (list col row) stop)
        (collect stop into coords)
        (leave coords))
      (for col initially (first start) then (+ col step-col))
      (for row initially (second start) then (+ row step-row))
      (collect (list col row) into coords))))


(defun make-rock-coordinates (rock-structures)
  "Convert line segment information into actual coordinate series."
  (iter outer
    (for rock-segment in rock-structures)
    (iter 
      (for stop in rock-segment)
      (for start previous stop)
      (when (null start)
        (next-iteration))
      (in outer
          (maximizing (max (first start) (first stop)) into cols)
          (maximizing (max (second start) (second stop)) into rows)
          (minimizing (min (first start) (first stop)) into min-cols)
          (minimizing (min (second start) (second stop)) into min-rows)
          (collect (make-coordinate-range start stop) into coords)))
    (finally
     ;; Add 1 to rows and cols; problem does 1-indexing, aref does 0-indexing.
     (return-from outer
       (list (list (1+ cols) (1+ rows) min-cols min-rows) coords)))))


(defun make-cave (rock-structure-sets)
  "Initialize the cave by marking air with . and rock with #."
  (destructuring-bind ((cols rows min-col min-row) rock-coordinate-sets) (make-rock-coordinates rock-structure-sets)
    ;(format t "Dimensions: ~a cols X ~a rows~%" cols rows)
    (let ((cave-grid (make-array (list cols rows)
                                 :element-type 'character
                                 :initial-element #\.
                                 :adjustable t)))
        (iter
          (for rock-coordinate-set in rock-coordinate-sets)
          (iter
            (for (col row) in rock-coordinate-set)
            (setf (aref cave-grid col row) #\#)))
      (setf (aref cave-grid (first +sand-source+) (second +sand-source+)) #\+)
      (list min-col min-row cave-grid))))


(defun print-cave (cave-data)
  "Print cave to screen."
  (destructuring-bind (min-col min-row cave-grid) cave-data
    (declare (ignorable min-row))
    (let* ((dimensions (array-dimensions cave-grid))
           (cols (first dimensions))
           (rows (second dimensions))
           (presentation-min-col
             (if (> min-col 1) (1- min-col) min-col)))
      (iter
        (for row from 0 below rows)
        (iter
          (for col from presentation-min-col below cols)
          (format t "~a" (aref cave-grid col row)))
        (format t "~%")))))


(defun drop-sand (cave-grid sand-source)
  "Drop one unit of sand from `sand-source` onto `cave-grid`.
   Iterate sand unit position according to puzzle rules until resting place or
   falling off grid. Return list (updated-cave-grid fell-off) where fell-off
   is T if sand unit fell off grid or NIL if it didn't.

   Drop rules are:
   1) Sand units emerges at position +sand-source+ which is (500 0)
   2) Sand units fall straight down from +sand-source, one grid step at a time.
      Next grid position downwards is either
      A) empty,
      B) outside the grid/drops off the edge,
      C) rock structure, or
      D) a previous sand unit.
   3) If next grid position is
      Case A: Record next sand unit position and loop/move sand unit down again.
      Case B: Return (cave-grid T)
      Case C: Update cave-grid with #\o at sand unit position and
              return (new-cave-grid NIL).
      Case D: If position down one and left one is empty, set that as next sand
              unit position and loop/move sand unit again.
              else if down-left is blocked, test if down-right position is empty.
              If down-right is empty, set that as next sand unit position and
              loop/move sand unit again. If down-right is blocked, sand unit
              comes to rest; update cave-grid with #\o at its current position
              and return (new-cave-grid NIL).

      Phew."
  (labels ((value-at (pos cave-grid max-col max-row)
         "Return value in `cave-grid` at position `pos` or :outside if `pos` is
          outside array dimensions."
             (destructuring-bind (col row) pos
               (cond
                 ((> 0 col) :outside)         ; Left of grid
                 ((> 0 row) :outside)         ; Above grid
                 ((>= col max-col) :outside)  ; Right of grid
                 ((>= row max-row) :outside)  ; Below grid
                 (t (aref cave-grid col row)))))
           (get-next-position (current-pos cave-grid max-col max-row)
             "Figure out next position of sand unit based on `current-pos`
              and `cave-grid` where `current-pos` is a (col row) integer
              tuple and `cave-grid` is the array describing the cave.
              Return list (next-pos state) where next-pos is a (col row)
              integer tuple and state is :fell-off if next-pos is outside
              `cave-grid`, :at-rest if sand-unit is blocked to move further,
              :at-source if sand-unit is at rest at same position as sand source or
              :in-motion if sand-unit has moved but remains inside `cave-grid`."
             (let* ((cur-col (first current-pos))
                    (cur-row (second current-pos))
                    (straight-down (list cur-col (1+ cur-row)))
                    (down-left (list (1- cur-col) (1+ cur-row)))
                    (down-right (list (1+ cur-col) (1+ cur-row)))
                    (val-straight-down (value-at straight-down cave-grid max-col max-row))
                    (val-down-left (value-at down-left cave-grid max-col max-row))
                    (val-down-right (value-at down-right cave-grid max-col max-row)))
               (cond
                 ((equal val-straight-down :outside) (list straight-down :fell-off)) 
                 ((equal val-straight-down #\.) (list straight-down :in-motion))
                 (t (progn  ;;; Test down-left
                      (cond
                        ((equal val-down-left :outside) (list down-left :fell-off))
                        ((equal val-down-left #\.) (list down-left :in-motion))
                        (t (progn  ;; Test down-right
                             (cond
                               ((equal val-down-right :outside) (list down-right :fell-off))
                               ((equal val-down-right #\.) (list down-right :in-motion))
                               (t (if (equal current-pos +sand-source+)
                                      (list current-pos :at-source)
                                      (list current-pos :at-rest)))))))))))))
    (let ((sand-unit-position sand-source)
          (cave-max-col (array-dimension cave-grid 0))
          (cave-max-row (array-dimension cave-grid 1)))
      (iter
        (for ((next-col next-row) state) = (get-next-position
                                            sand-unit-position
                                            cave-grid
                                            cave-max-col
                                            cave-max-row))
        (for (cur-col cur-row) = sand-unit-position)
        (case state
          (:fell-off (leave (list cave-grid T)))
          (:at-rest (progn
                      (setf (aref cave-grid cur-col cur-row) #\o)
                      (leave (list cave-grid nil))))
          (:at-source (leave (list cave-grid T))))
        (setf sand-unit-position (list next-col next-row))))))


(defun adjust-cave (cave)
  "Add two rows and an 'infinite' floor for part 2 of problem."
  (let* ((min-col (first cave))
         (min-row (first cave))
         (old-cave-grid (third cave))
         (old-cave-grid-dims (array-dimensions old-cave-grid))
         (old-cave-grid-cols (first old-cave-grid-dims))
         (old-cave-grid-rows (second old-cave-grid-dims))
         (new-cave-grid-cols (+ 150 old-cave-grid-cols))  ; 150 to simulate wide enough cave floor.
         (new-cave-grid-rows (+ 2 old-cave-grid-rows))
         (new-cave-grid (adjust-array
                         old-cave-grid
                         `(,new-cave-grid-cols ,new-cave-grid-rows)
                         :initial-element #\.)))
    ;; Add a floor
    (iter
      (for row = (1- new-cave-grid-rows))
      (for col from 0 below new-cave-grid-cols)
      (setf (aref new-cave-grid col row) #\#))
    (list min-col min-row new-cave-grid)))


(defun solve-part-1 (rock-structures)
  "Solve part 1."
  (let* ((cave (make-cave rock-structures))
         (min-col (first cave))
         (min-row (second cave))
         (cave-grid (third cave))
         (after-sand-drop
           (iter
             (for sand-units initially 0 then (1+ sand-units))
             (for next-state initially (list cave-grid nil) then (drop-sand cave-grid +sand-source+))
             (when (second next-state)  ; first sand-unit to fall off.
               (leave (list (first next-state) (1- sand-units)))))))
    (format t "~a units of sand dropped before falling off.~%" (second after-sand-drop))
    (print-cave (list min-col min-row (first after-sand-drop)))
    (second after-sand-drop)))


(defun solve-part-2 (rock-structures)
  "Solve part 2."
  (let* ((initial-cave (make-cave rock-structures))
         (adjusted-cave (adjust-cave initial-cave))
         (min-col (first adjusted-cave))
         (min-row (second adjusted-cave))
         (cave-grid (third adjusted-cave))
         (after-sand-drop
           (iter
             (for sand-units initially 0 then (1+ sand-units))
             (for next-state initially (list cave-grid nil) then (drop-sand cave-grid +sand-source+))
             (when (second next-state)  ; first sand-unit to fall off.
               (leave (list (first next-state) sand-units))))))
    (print-cave (list (- min-col 150) min-row (first after-sand-drop)))  ; 150 to adjust presentation of final cave.
    (second after-sand-drop)))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 12."
  (let* ((raw-input-data (get-input #P"./input"))
         (rock-structures (parse-rock-structures raw-input-data)))
    (let ((part-1 (solve-part-1 rock-structures))
          (part-2 (solve-part-2 rock-structures)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

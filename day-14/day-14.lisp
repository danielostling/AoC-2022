;;;; day-14.lisp

(in-package #:day-14)

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
          (maximizing right-distance into max-width)
          (maximizing down-distance into max-height)))
    (finally
     (return-from outer (list max-width max-height)))))


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
          (maximizing (max (first start) (first stop)) into width)
          (maximizing (max (second start) (second stop)) into height)
          (minimizing (min (first start) (first stop)) into min-width)
          (minimizing (min (second start) (second stop)) into min-height)
          (collect (make-coordinate-range start stop) into coords)))
    (finally
     ;; Add 1 to height and width; problem does 1-indexing, aref does 0-indexing.
     (return-from outer
       (list (list (1+ height) (1+ width) min-height min-width) coords)))))


(defconstant +sand-source+ '(500 0))


(defun make-cave (rock-structure-sets)
  "Initialize the cave by marking air with . and rock with #."
  (destructuring-bind ((height width min-height min-width) rock-coordinate-sets) (make-rock-coordinates rock-structure-sets)
    (format t "dimensions: ~a X ~a~%" width height)
    (let ((cave-grid (make-array (list height width)
                                 :element-type 'character
                                 :initial-element #\.)))
        (iter
          (for rock-coordinate-set in rock-coordinate-sets)
          (iter
            (for (col row) in rock-coordinate-set)
            (setf (aref cave-grid row col) #\#)))
      (setf (aref cave-grid (second +sand-source+) (first +sand-source+)) #\+)
      (list min-height min-width cave-grid))))


(defun print-cave (cave-data)
  "Print cave to screen."
  (destructuring-bind (min-height min-width cave-grid) cave-data
    (declare (ignorable min-height))
    (let* ((dimensions (array-dimensions cave-grid))
           (rows (first dimensions))
           (cols (second dimensions))
           (presentation-min-width
             (if (> min-width 1) (1- min-width) min-width)))
      (iter
        (for row from 0 below rows)
        (iter
          (for col from presentation-min-width below cols)
          (format t "~a" (aref cave-grid row col)))
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
  (labels ((value-at (pos cave-grid max-row max-col)
         "Return value in `cave-grid` at position `pos` or :outside if `pos` is
          outside array dimensions."
             (destructuring-bind (row col) pos
               (cond
                 ((< 0 row) :outside)
                 ((< 0 col) :outside)
                 ((> row max-row) :outside)
                 ((> col max-col) :outside)
                 (t (aref cave-grid row col)))))
           (get-next-position (current-pos cave-grid max-row max-col)
             "Figure out next position of sand unit based on `current-pos`
            and `cave-grid` where `current-pos` is a (row col) integer
            tuple and `cave-grid` is the array describing the cave.
            Return list (next-pos state) where next-pos is a (row col)
            integer tuple and state is :fell-off if next-pos is outside
            `cave-grid`, :at-rest if sand-unit is blocked to move further or
            :in-motion if sand-unit has moved but remains inside `cave-grid`."
           (let* ((straight-down (list (1+ (first current-pos)) (second current-pos)))
                  (down-left (list (1+ (first current-pos)) (1- (second current-pos))))
                  (down-right (list (1+ (first current-pos)) (1+ (second current-pos))))
                  (val-straight-down (value-at straight-down cave-grid max-row max-col))
                  (val-down-left (value-at down-left cave-grid max-row max-col))
                  (val-down-right (value-at down-right cave-grid max-row max-col)))
             (cond
               ((equal val-straight-down :outside) (list straight-down :fell-off)) 
               ((equal val-straight-down #\.) (list straight-down :in-motion))
               ((equal val-straight-down #\#) (list straight-down :at-rest))
               ((equal val-down-left :outside) (list down-left :fell-off))
               ((equal val-down-left #\.) (list down-left :in-motion))
               ((equal val-down-left #\#) (list down-left :at-rest))
               ((equal val-down-right :outside) (list down-right :fell-off))
               ((equal val-down-right #\.) (list down-right :in-motion))
               ((equal val-down-right #\#) (list down-right :at-rest))
               (t (list current-pos :at-rest))))
             )
           )
    (let ((sand-unit-position sand-source)
          (cave-max-row (array-dimension cave-grid 0))
          (cave-max-col (array-dimension cave-grid 1)))
      (iter
        (for ((next-row next-col) state) = (get-next-position
                                      sand-unit-position
                                      cave-grid
                                      cave-max-row
                                      cave-max-col))
        (case state
          (:fell-off (leave (list cave-grid T)))
          (:at-rest (progn
                      (setf (aref cave-grid next-row next-col) #\o)
                      (leave (list cave-grid NIL)))))
        (setf sand-unit-position (list next-row next-col))))
    )
  )


(defun solve-part-1 (rock-structures)
  "Solve part 1."
  (let* ((cave-array (make-cave rock-structures)))
    (format t "~a~%" cave-array)))


(defun solve-part-2 (rock-structures)
  "Solve part 2."
  (declare (ignorable rock-structures))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 12."
  (let* ((raw-input-data (get-input #P"./input-example"))
         (rock-structures (parse-rock-structures raw-input-data))
         )
    (let ((part-1 (solve-part-1 rock-structures))
          (part-2 (solve-part-2 rock-structures)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

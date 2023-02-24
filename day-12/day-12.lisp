;;;; day-12.lisp

(in-package #:day-12)


(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun assoc-v (item alist &key (default 0))
  "Get just the value, not the key also."
  (let ((pair (assoc item alist :test #'equal)))
    (cond ((null pair) default)
          (t (cdr pair)))))


(defun insert-or-update (key val alist)
  "Insert `val` into `alist` at `key`, or replace existing value for `key` in
   `alist` with `val`. Return updated `alist`."
  (let* ((new-alist (copy-alist alist))
         (key-place (assoc key new-alist)))
    (cond 
          ((null key-place) (push (cons key val) new-alist))
          (t (setf (cdr key-place) val)))
    new-alist))


(defun parse-topology (raw-topology)
  "Parse `raw-topology`."
  (let* ((char-lists (iter
                  (for row in raw-topology)
                       (collect (coerce row 'list))))
         (start-point '(0 0))
         (goal-point '(0 0))
         (goal-point-elevation (- (char-code #\z) (char-code #\a)))  ; Z from instructions.
         (int-lists (iter outer
                      (for char-row in char-lists)
                      (for arr-row first 0 then (1+ arr-row))
                      (for num-row = (iter
                                       (for c in char-row)
                                       (for arr-col first 0 then (1+ arr-col))
                                       (for value = (cond ((char= c #\S)
                                                           (progn
                                                             (setf start-point (list arr-row arr-col))
                                                             0))
                                                          ((char= c #\E)
                                                           (progn
                                                             (setf goal-point (list arr-row arr-col))
                                                             goal-point-elevation))
                                                          (t (- (char-code c) (char-code #\a)))))
                                       (collect value)))
                      (collect num-row))))
    (values (make-array (list (length int-lists) (length (first int-lists)))
                        :initial-contents int-lists)
            start-point
            goal-point)))


(defun manhattan-distance (pos1 pos2)
  "Calculate manhattan distance between (x y) pairs `pos1` `pos2`.
   Distance is |x1 - x2| + |y1 - y2|."
  (+
   (abs
    (- (first pos1) (first pos2)))
   (abs
    (- (second pos1) (second pos2)))))


(defun elevation-at (pos topology)
  "Return elevation at position `pos` in `topology`."
  (aref topology (first pos) (second pos)))


(defun get-neighbors (current-pos topology)
  "Given a position `current-pos` (a (x y) pair) in `topology`, return list of
   neighbors as a list of (x y) pairs.

   Rules;
   1. Neighbor positions are exactly one step up, right, down or left,
   2. Neighbor can at most be + or - one unit of elevation compared to height
      of current position.
   3. Implied; don't move outside of the topology grid."
  (flet ((possible-neighbor-p (current-pos next-pos topology)
           "Return T if elevation difference between `current-pos` and `next-pos` is
            1 or less, else return nil."
           (if (> (- (elevation-at next-pos topology)
                     (elevation-at current-pos topology))
                  1)
               nil
               t)))
    (let* ((current-pos-row (first current-pos))
           (current-pos-col (second current-pos))
           (topology-max-row (1- (array-dimension topology 0)))
           (topology-max-col (1- (array-dimension topology 1)))
           (neighbors nil))
      (push (list (1- current-pos-row) current-pos-col) neighbors)  ; Up
      (push (list current-pos-row (1+ current-pos-col)) neighbors)  ; Right
      (push (list (1+ current-pos-row) current-pos-col) neighbors)  ; Down
      (push (list current-pos-row (1- current-pos-col)) neighbors)  ; Left
      (iter
        (for neighbor in neighbors)
        (when (or (> (first neighbor) topology-max-row)
                  (< (first neighbor) 0)
                  (> (second neighbor) topology-max-col)
                  (< (second neighbor) 0))
          (next-iteration))
        (if (possible-neighbor-p current-pos neighbor topology)
            (collect neighbor))))))


(defun render-path (path topology)
  "Draw `path` in `topology`."
  (flet ((convert-to-char (n)
           "Convert `n` into a char and return it, where n=0 represents #\a."
           (code-char (+ n (char-code #\a)))))
    (let ((rendering (make-array (array-dimensions topology))))
      (iter
        (for row from 0 below (array-dimension topology 0))
        (iter
          (for col from 0 below (array-dimension topology 1))
          (setf (aref rendering row col) (convert-to-char (aref topology row col)))))
      (iter
        (for step in path)
        (for (row-step col-step) = step)
        (for previous-step previous step)
        (setf (aref rendering row-step col-step) #\*)
        (when (null previous-step)
          (next-iteration))
        (for (row-prev col-prev) = previous-step)
        (setf (aref rendering row-prev col-prev) #\+))
      rendering)))


(defun reconstruct-path (came-from current-pos)
  "Build path from `came-from` and `current-pos` generated in `a-star-search.`"
  (let ((total-path (list current-pos))
        (came-from-keys (iter (for (key . nil) in came-from) (collect key))))
    (iter
      (when (not (member current-pos came-from-keys :test #'equal))
        (leave total-path))
      (setf current-pos (assoc-v current-pos came-from))
      (push current-pos total-path))
    total-path))


(defun lowest-score (open-set f-score)
  "Return pos in `open-set` with lowest `f-score` value."
  (let ((sorted nil))
    (iter
      (for pos in open-set)
      (collect (cons pos (assoc-v pos f-score)) into candidates)
      (finally (setf sorted candidates)))
    (setf sorted (sort sorted #'< :key #'cdr))
    (first sorted)))


(defun a-star-search (start-pos goal-pos topology)
  "Find the shortest path from `start-pos` to `goal-pos` in grid `topology`.
  Return path as steps in list of (x y) positions."
  (let* ((open-set (list start-pos))
         (came-from nil)
         (g-score (list (cons start-pos 0)))
         (f-score (list (cons start-pos (manhattan-distance start-pos goal-pos))))
         (current-pos nil)
         (path nil)
         (step-counter 0))
    (iter
      (incf step-counter)
      (when (null open-set)
        (format t "open-set became empty, could not reach goal.~%")
        (leave))
      (setf current-pos (car (lowest-score open-set f-score)))
      (when (equal current-pos goal-pos)
        (format t "goal reached~%")
        (setf path (reconstruct-path came-from current-pos))
        (finish))
      (setf open-set (remove current-pos open-set :test #'equal))
      (iter
        (for neighbor in (get-neighbors current-pos topology))
        (for tentative-g-score = (+ (assoc-v current-pos g-score) 1)) ;; weight is 1
        (when (< tentative-g-score (assoc-v neighbor g-score :default 1000000))
          (setf came-from (insert-or-update neighbor current-pos came-from))
          (setf g-score (insert-or-update neighbor tentative-g-score g-score))
          (setf f-score (insert-or-update neighbor
                                          (+ tentative-g-score
                                             (manhattan-distance neighbor
                                                                 goal-pos))
                                          f-score))
          (pushnew neighbor open-set :test #'equal))))
    path))


(defun solve-part-1 (topology start-pos goal-pos)
  "Solve part 1."
  (1- (length (a-star-search start-pos goal-pos topology))))


(defun solve-part-2 (topology start-pos goal-pos)
  "Solve part 2.

   NOTE: This is a pretty bad bruteforce solution. A better approach would have
   to have a-star-search return a list of a-elevation points it passes so each
   a-elevation point is only calculated once."
  (declare (ignorable start-pos))
  (let ((starting-positions
          (iter outer
            (for row from 0 below (array-dimension topology 0))
            (iter
              (for col from 0 below (array-dimension topology 1))
              (when (= 0 (aref topology row col))
                (in outer
                    (collect (list row col))))))))
    (iter
      (for starting-pos in starting-positions)
      (for steps = (solve-part-1 topology starting-pos goal-pos))
      (when (> steps -1)
        (minimizing steps)))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 12."
  (let* ((raw-topology (get-input #P"./input")))
    (multiple-value-bind (topology start-pos goal-pos) (parse-topology raw-topology)
      (let ((part-1 (solve-part-1 topology start-pos goal-pos))
            (part-2 (solve-part-2 topology start-pos goal-pos)))
        (format t "First part: ~a~%" part-1)
        (format t "Second part: ~a~%" part-2)))))

;;;; day-2.lisp
;;; Rather naïve solution, I'm afraid.

(in-package #:day-2)


(defun get-input (path)
  "Read puzzle input from path, return as list of lines."
  (uiop:read-file-lines path))


(defun shape-symbol (shape-string)
  "Return shape symbol and its score value based on shape string.

   A, X: Rock => 1
   B, Y: Paper => 2
   C, Z: Scissors => 3"
  (cond ((string= "A" shape-string) (values :ROCK 1))
        ((string= "X" shape-string) (values :ROCK 1))
        ((string= "B" shape-string) (values :PAPER 2))
        ((string= "Y" shape-string) (values :PAPER 2))
        ((string= "C" shape-string) (values :SCISSORS 3))
        ((string= "Z" shape-string) (values :SCISSORS 3))))


(defun compare-shapes1 (shape-a shape-b)
  "Compare shapes, returning integer value.

   :ROCK     > :SCISSORS => 6
   :ROCK     = :ROCK     => 3
   :ROCK     < :PAPER    => 0
   :SCISSORS = :SCISSORS => 3
   :SCISSORS < :PAPER    => 0
   :PAPER    = :PAPER    => 3"
  (if (eq shape-a shape-b) 3
  (cond
    ((and (eq shape-a :ROCK) (eq shape-b :SCISSORS)) 6)
    ((and (eq shape-a :ROCK) (eq shape-b :PAPER)) 0)
    ((and (eq shape-a :PAPER) (eq shape-b :ROCK)) 6)
    ((and (eq shape-a :PAPER) (eq shape-b :SCISSORS)) 0)
    ((and (eq shape-a :SCISSORS) (eq shape-b :ROCK)) 0)
    ((and (eq shape-a :SCISSORS) (eq shape-b :PAPER)) 6))))


(defun round-outcome1 (opponent-shape-code my-shape-code)
  "Return part 1 score for your result as an integer based on opponent and own shape selection.

   Shape values
   ------------
   A, X: Rock     => 1
   B, Y: Paper    => 2
   C, Z: Scissors => 3

   Outcome values
   --------------
   Loss => 0
   Win  => 6
   Draw => 3

   Total round score equals shape value + outcome value."
  (multiple-value-bind (my-shape my-shape-value) (shape-symbol my-shape-code)
    (+ my-shape-value
       (let* ((opponent-shape (shape-symbol opponent-shape-code))
              (outcome-value (compare-shapes1 my-shape opponent-shape)))
         outcome-value))))


(defun calculate-my-move (opponent-shape expected-outcome)
  "Determine my move based on opponent shape and expected outcome.

   Expected  Should
   outcome   return
   --------------------------------
   X (Lose)  Losing move
   Y (Draw)  Same as opponent-shape
   Z (Win)   Winning move

   In order to re-use the shape-symbol function, return shape code instead of shape:

   Rock     => A
   Paper    => B
   Scissors => C"

  (flet ((calculate-draw (shape)
           "Return draw string based on shape symbol."
           (case shape
             (:ROCK "A")
             (:PAPER "B")
             (:SCISSORS "C"))))
    (if (string= expected-outcome "Y")
        (calculate-draw opponent-shape)
        (cond
          ((string= expected-outcome "X") (case opponent-shape
                                            (:ROCK "C")
                                            (:PAPER "A")
                                            (:SCISSORS "B")))
          ((string= expected-outcome "Z")  (case opponent-shape
                                             (:ROCK "B")
                                             (:PAPER "C")
                                             (:SCISSORS "A")))))))


(defun round-outcome2 (opponent-shape-code expected-outcome)
  "Return part 2 score for your result as an integer based on opponent shape selection and expected outcome.

   Shape values
   ------------
   A: Rock     => 1
   B: Paper    => 2
   C: Scissors => 3

   Expected outcomes
   -----------------
   X: Lose
   Y: Draw
   Z: Win

   Outcome values
   --------------
   Loss => 0
   Win  => 6
   Draw => 3

   Total round score equals shape value + outcome value."
  (let* ((opponent-shape (shape-symbol opponent-shape-code))
         (my-shape-code (calculate-my-move opponent-shape expected-outcome)))
    (multiple-value-bind (my-shape my-shape-value) (shape-symbol my-shape-code)
      (+ my-shape-value (compare-shapes1 my-shape opponent-shape)))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 2."
  (let* ((moves (get-input "./input"))
         (part-1 (reduce
                  #'+
                  (iter
                    (for move-string in moves)
                    (for opponent-move = (subseq move-string 0 1))
                    (for my-move = (subseq move-string 2 3))
                    (collect (round-outcome1 opponent-move my-move)))))
         (part-2 (reduce
                  #'+
                  (iter
                    (for move-string in moves)
                    (for opponent-move = (subseq move-string 0 1))
                    (for expected-outcome = (subseq move-string 2 3))
                    (collect (round-outcome2 opponent-move expected-outcome))))))
    
    (format t "Day 2, Part 1: ~a~%" part-1)
    (format t "Day 2, Part 2: ~a~%" part-2)))

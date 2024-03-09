;;;; day-16.lisp

(in-package #:day-16)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-valves-flows-paths (input)
  "Parse `input` into a graph representation of the problem.
   Each node is represented by two hashes; one for flow rate and one for
   connections to other valves.

   Turns the string
     'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB'
   into flow rate hash kv-pair
     {'AA': 0}
   and connections hash kv-pair
     {'AA': ('DD' 'II' 'BB')}.

   Return list (flow-rate connections)."
  (let ((flow-rate (make-hash-table :test #'equal))
        (connections (make-hash-table :test #'equal)))
    (iter
      (for line in input)
      (for parts = (uiop:split-string line :separator '(#\Space #\= #\; #\,)))
      (for valve = (nth 1 parts))
      (for flow = (parse-integer (nth 5 parts)))
      (for reachable = (remove "" (subseq parts 11) :test #'equal))
      ;; Adding current valve as reachable represent decision to not move.
      ;;(for reachable-and-self = (pushnew valve reachable :test #'equal))
      (setf (gethash valve flow-rate) flow)
      (setf (gethash valve connections) reachable))
    (list flow-rate connections)))


(defun total-flow (starting-valve action-sequence flow-rate)
  "Given `starting-valve` and the list `action-sequence` representing actions
   taken at each minute, and `flow-rate` describing how much flow each valve
   represents, calculate the total flow. Each element in `action-sequence` is
   either 1) :open-valve, or 2) valve-name (representing a move to that valve)."
  (let ((flow-sum 0)
        (current-flow 0)
        (current-valve starting-valve))
    (iter
      (for move-or-open in action-sequence)
      (incf flow-sum current-flow)
      (if (equal move-or-open :open-valve)
          (incf current-flow (gethash current-valve flow-rate))
          (setf current-valve move-or-open)))
    flow-sum))


(defun test (flows-connections)
  "Test case for example given in puzzle description part 1."
  (let ((sequence
          '("DD" :open-valve "CC" "BB" :open-valve
           "AA" "II" "JJ" :open-valve "II"
           "AA" "DD" "EE" "FF" "GG"
           "HH" :open-valve "GG" "FF" "EE"
           :open-valve "DD" "CC" :open-valve "CC"
           "CC" "CC" "CC" "CC" "CC")))
    (total-flow "AA" sequence (first flows-connections))))


(defun iterative-deepening-search (current-valve remaining-time
                                   flow-rate connections &optional (paths nil))
  "Find all `paths` satifying criteria.
  Start the search at `current-valve` and perform an iterative deepening search
  in the graph described by `connections`.

  At each step, take one of these actions
  1. Do nothing, costing one minute,
  2. Move to a connected valve, costing one minute, or
  3. Stay and open current valve, costing one minute.   

  When `remaining-time` equals zero, stop traversing."

  (iter
    ;;(for depth initially 0 then (1+ depth))
    (for depth from 0 to 6)
    (format t "New path set starting for depth ~a~%" depth)
    (for (found remaining) = (depth-limited-dfs current-valve depth connections (list "HH")))
    (when found (leave found))
    (when (null remaining) (leave nil))))


(defun depth-limited-dfs (valve depth connections goals &optional (path nil))
  "Do a depth limited depth-first search through `connections`, starting at
  `valve`. Return goal valve when reached, or nil if `depth` is reached."
  (if (= depth 0)
      (if (member valve goals :test #'equal) (list valve t) (list nil t))
      (let ((any-remaining nil))
        (iter
          (for reachable-valve in (gethash valve connections))
          (format t "At valve ~a, checking next valve ~a~%" valve reachable-valve)
          (for (found remaining) = (depth-limited-dfs
                                    reachable-valve
                                    (1- depth)
                                    connections
                                    goals
                                    (push reachable-valve path)))
          (when found (leave (list found t)))
          (when remaining (setf any-remaining t))
          (finally
           (return (list nil any-remaining)))))))


(defun solve-part-1 (valves-flows-paths)
  "Solve part 1."
  (declare (ignorable valves-flows-paths))
  "TBD")


(defun solve-part-2 (valves-flows-paths)
  "Solve part 2."
  (declare (ignorable valves-flows-paths))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 16."
  (let* ((raw-input-data (get-input #P"./input-example"))
         (valves-flows-paths (parse-valves-flows-paths raw-input-data)))
    (let ((part-1 (solve-part-1 valves-flows-paths))
          (part-2 (solve-part-2 valves-flows-paths)))
      (format t "First part: ~a~%" part-1)
      (format t "Second part: ~a~%" part-2))))

;;;; day-11.lisp

(in-package #:day-11)


(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun assoc-v (item alist &key (test #'equal))
  "Return value of `item` in `alist`."
  (cdr (assoc item alist :test test)))


(defun get-least-common-multiple (statements)
  "Compute LCM of all test division divisors and return it."
  (apply #'lcm
   (iter outer
     (for (nil . rule) in statements)
     (iter
       (for (part . value) in rule)
       (when (string-equal part "divisor-test")
         (in outer (appending value)))))))


(defun split-and-trim (raw-line)
  "Split `raw-line` by #\: and trim off whitespace. Return (left right) parts."
  (destructuring-bind
      (left-part right-part)
      (uiop:split-string raw-line :separator '(#\:))
    (let ((left (string-trim '(#\Space) left-part))
          (right (string-trim '(#\Space) right-part)))
      (list left right))))


(defun parse-starting-items (items-string)
  "Parse string '95, 72, 98, 82, 86' into (95 72 98 82 86)."
  (mapcar #'parse-integer
          (remove-if
           #'(lambda (elem) (= (length elem) 0))
           (uiop:split-string items-string :separator '(#\, #\Space)))))


(defun parse-operation (operation-string)
  "Parse string 'new = old * 3' into ('old' #'* 3)."
  (let* ((parts (uiop:split-string operation-string))
         (right (subseq parts (- (length parts) 3))))
    (iter
      (for term in right)
      (collect
          (cond ((string-equal term "*") #'*)
                ((string-equal term "+") #'+)
                ((string-equal term "old") term)
                (t (parse-integer term)))))))


(defun parse-get-last-integer (raw-string)
  "Parse string 'word-1 word-2 word-n value' into integer value."
  (parse-integer (car (last (uiop:split-string raw-string)))))


;; (defun get-stack (statements)
;;   "Extract item stack from statement list."
;;   (iter
;;     (for (left right) in statements)
;;     (when (string-equal left "Starting items")
;;       (leave (parse-starting-items right)))))


(defun next-monkey (item worry-reduction-factor throw-rules test-lcm)
  "Calculate decision for which monkey to throw item to based on `item`,
   `worry-reduction-factor` and `throw-rules`."
  (let ((operation nil)
        (divisor-test nil)
        (if-true nil)
        (if-false nil)
        (left-term nil)
        (op nil)
        (right-term nil)
        (new-worry-level nil))
    (iter
      (for (left . right) in throw-rules)
      (cond ((string-equal left "operation")
             (setf operation (car right)))
            ((string-equal left "divisor-test")
             (setf divisor-test (car right)))
            ((string-equal left "if-true")
             (setf if-true (car right)))
            ((string-equal left "if-false")
             (setf if-false (car right)))
            (t nil)))
    (setf left-term (if (stringp (first operation)) item (first operation)))
    (setf op (second operation))
    (setf right-term (if (stringp (third operation)) item (third operation)))
    (setf new-worry-level (floor (/ (funcall op left-term right-term) worry-reduction-factor)))
    (setf new-worry-level (mod new-worry-level test-lcm))
    (if (zerop (rem new-worry-level divisor-test))
        (list new-worry-level (format nil "Monkey ~a" if-true))
        (list new-worry-level (format nil "Monkey ~a" if-false)))))


(defun add-statement-to-monkey (monkey statements new-statement)
  "Add `new-statement` to `monkey` in `statements`.
   Return updated statements."
  (let* ((new-statements (copy-seq statements))
         (monkey-statements (assoc monkey new-statements :test #'string-equal)))
    (if (null monkey-statements)
        (push (cons monkey (list new-statement)) new-statements)
        (setf (cdr monkey-statements)
              (append (cdr monkey-statements) (list new-statement))))
    new-statements))


(defun refine-statement (statement)
  "Convert a statement into the relevant information per statement type."
  (destructuring-bind (left right) statement
    (cond ((string-equal left "Starting items")
           (list "holding-items" (parse-starting-items right)))
          ((string-equal left "Operation")
            (list "operation" (parse-operation right)))
          ((string-equal left "Test")
           (list "divisor-test" (parse-get-last-integer right)))
          ((string-equal left "If true")
           (list "if-true" (parse-get-last-integer right)))
          ((string-equal left "If false")
           (list "if-false" (parse-get-last-integer right)))
          (t (list "unknown statement" statement)))))


(defun refine-statements (statements)
  "Extract relevant parts from statements and return updated statements."
  (let ((new-statements nil))
    (iter
      (for (monkey . monkey-statements) in statements)
      (push
       (cons monkey (mapcar #'refine-statement monkey-statements))
       new-statements))
    new-statements))


(defun parse-statements (input)
  "Convert the game statement rules into a data structure."
  (let ((current-monkey nil)
        (statements nil))
    ;; Group statements by monkey.
    (iter
      (for line in input)
      (when (= (length line) 0)
        (next-iteration))
      (for (left-part right-part) = (split-and-trim line))
      (when (uiop:string-prefix-p "Monkey " left-part)
        (setf current-monkey left-part)
        (next-iteration))
      (setf statements
            (add-statement-to-monkey
             current-monkey
             statements
             (list left-part right-part))))
    (refine-statements statements)))


(defun get-items (monkey held-items)
  "Return items currently held by `monkey`."
  (assoc-v monkey held-items))


(defun push-item-to-end (monkey item held-items)
  "Add `item` to end of held items list for `monkey`.
   Return updated statements."
  (let* ((new-held-items (copy-alist held-items))
         (monkey-items (assoc-v monkey new-held-items)))
    (setf monkey-items (append monkey-items (list item)))
    (setf (cdr (assoc monkey new-held-items :test #'string-equal)) monkey-items)
    new-held-items))


(defun pop-item (monkey held-items)
  "Pop item from `held-items` list for `monkey`.
   Return list (item updated-items)."
  (let* ((new-held-items (copy-alist held-items))
         (monkey-items (assoc-v monkey new-held-items))
         (popped-item nil))
    (setf popped-item (pop monkey-items))
    (setf (cdr (assoc monkey new-held-items :test #'string-equal)) monkey-items)
    (list popped-item new-held-items)))


(defun throw-item (throwing-monkey receiving-monkey item-to-throw held-items)
  "Throw `item-to-throw` from `throwing-monkey` to `receiving-monkey`.
   Return updated `held-items`. Note that `item-to-throw` is calculated outside
   of this function."
  (destructuring-bind
      (thrown-item new-held-items)
      (pop-item throwing-monkey held-items)
    (if (null thrown-item)
        held-items
        (push-item-to-end receiving-monkey item-to-throw new-held-items))))


(defun get-throw-rules (monkey throw-rules)
  "Return throw rules for `monkey`."
  (assoc-v monkey throw-rules))


(defun update-throw-count (monkey item-count item-inspection-count)
  "Add `item-count` for `monkey` in `item-inspection-count`.
   Return updated `item-inspection-count`."
  (let* ((new-item-inspection-count (copy-alist item-inspection-count))
         (cur-count (assoc-v monkey new-item-inspection-count))
         (new-count (+ cur-count item-count)))
    (setf (cdr (assoc monkey new-item-inspection-count)) new-count)
    new-item-inspection-count))


(defun perform-rounds (held-items
                       monkey-throw-rules
                       monkeys
                       rounds
                       worry-reduction-factor
                       test-lcm)
  "Throw item(s) around for one set of rounds.
   Return item inspect count structure.

   `held-items` - alist of items held by monkey.
   `monkey-throw-rules` - item routing rules by monkey.
   `monkeys` - list of monkey names.
   `rounds` - how many rounds to perform.
   `worry-reduction-factor` - divisor to reduce worry level.
   `test-lcm` - least common multiple for rule division tests."
  (let ((item-inspection-count
          (iter
            (for monkey in monkeys)
            (collect (cons monkey 0)))))
    (iter
      (for round from 1 to rounds)
      (iter
        (for throwing-monkey in monkeys)
        (for items-to-throw = (get-items throwing-monkey held-items))
        (setf item-inspection-count (update-throw-count
                                     throwing-monkey
                                     (length items-to-throw)
                                     item-inspection-count))
        (iter
          (for idx from 0 below (length items-to-throw))
          (for item-to-throw = (first (get-items throwing-monkey held-items)))
          (for throw-info = (next-monkey
                             item-to-throw
                             worry-reduction-factor
                             (assoc-v throwing-monkey monkey-throw-rules)
                             test-lcm))
          (for updated-item-to-throw = (first throw-info))
          (for receiving-monkey = (second throw-info))
          (setf held-items (throw-item
                            throwing-monkey
                            receiving-monkey
                            updated-item-to-throw
                            held-items)))))
    item-inspection-count))


(defun solve-part-1 (statements rounds worry-reduction-factor)
  "Solve part 1."
  (let ((monkeys (sort (mapcar #'car statements) #'string<))
        (held-items nil)
        (monkey-throw-rules nil)
        (item-inspection-count nil)
        (least-common-multiple nil))
    ;; Set up held-items, alist with monkey as key, held items list as value,
    ;; and set up monkey-throw-rules, alist with monkey as key, throw rules
    ;; alist as value. Also set up the item inspection counter.
    (iter
      (for (monkey . monkey-statements) in statements)
      (for items = (car (assoc-v "holding-items" monkey-statements)))
      (for just-rules = (remove "holding-items" monkey-statements :key #'car :test #'string-equal))
      (push (cons monkey items) held-items)
      (push (cons monkey just-rules) monkey-throw-rules)
      (push (cons monkey 0) item-inspection-count))
    ;; Calculate LCD of division part of behavior rules.
    (setf least-common-multiple (get-least-common-multiple statements))
    ;; Do the rounds of item-throwing.
    (setf item-inspection-count
          (perform-rounds
           held-items
           monkey-throw-rules
           monkeys
           rounds
           worry-reduction-factor
           least-common-multiple))
    ;; Finally, calculate the multiplication of the two largest inspection counts.
    (let* ((sorted-throw-count (sort item-inspection-count #'> :key #'cdr))
           (term1 (cdr (first sorted-throw-count)))
           (term2 (cdr (second sorted-throw-count))))
      (* term1 term2))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 11."
  (let* ((raw-statements (get-input #P"./input"))
         (statements (parse-statements raw-statements))
         (part-1 (solve-part-1 statements 20 3))
         (part-2 (solve-part-1 statements 10000 1)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

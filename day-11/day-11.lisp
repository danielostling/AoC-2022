;;;; day-11.lisp

(in-package #:day-11)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun assoc-v (item alist &key (test #'equal))
  "Return value of `item` in `alist`."
  (cdr (assoc item alist :test test)))


(defun split-and-trim (raw-line)
  "Split `raw-line` by : and trim off whitespace. Return (left right) parts."
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


(defun get-stack (statements)
  "Extract item stack from statement list."
  (iter
    (for (left right) in statements)
    (when (string-equal left "Starting items")
      (leave (parse-starting-items right)))))


(defmacro next-monkey (statements)
  "Convert textual description of a function into an actual function.
   The problem description also states there is an additional step between
   operation step and test step; (floor <operation result divided by 3>)

   Example statements
   (('Operation' 'new = old * 17')
    ('Test' 'divisible by 19')
    ('If true' 'throw to monkey 5')
    ('If false' 'throw to monkey 3'))
   
   should convert into
   (lambda (x)
     (if (zerop (rem (floor (/ (* x 17) 3)) 19)) 5 3))"
  ;; Step 1, parse the text input.
  (let ((operation (gensym))
        (divisor-test (gensym))
        (if-true (gensym))
        (if-false (gensym))
        (op-var (gensym))
        (left-term (gensym))
        (op (gensym))
        (right-term (gensym)))
    (iter
      (for (left right) in statements)
      (cond ((string-equal left "Operation")
             (setf operation (parse-operation right)))
            ((string-equal left "Test")
             (setf divisor-test (parse-get-last-integer right)))
            ((string-equal left "If true")
             (setf if-true (parse-get-last-integer right)))
            ((string-equal left "If false")
             (setf if-false (parse-get-last-integer right)))
            (t nil)))
    (setf left-term (if (stringp (first operation)) op-var (first operation)))
    (setf op (second operation))
    (setf right-term (if (stringp (third operation)) op-var (third operation)))
    `(lambda (,op-var)
       (if (zerop (rem (floor (/ (,op ,left-term ,right-term) 3)) ,divisor-test))
           ,if-true
           ,if-false))))


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


(defun move-item (throwing-monkey receiving-monkey held-items)
  "Throw one item from `throwing-monkey` to `receiving-monkey`.
   Return updated `held-items`."
  (destructuring-bind
      (thrown-item new-held-items)
      (pop-item throwing-monkey held-items)
    (if (null thrown-item)
        held-items
        (push-item-to-end receiving-monkey thrown-item new-held-items))))


(defun get-throw-rules (monkey throw-rules)
  "Return throw rules for `monkey`."
  (assoc-v monkey throw-rules))


(defun get-next-monkey (monkey statements)
  "Return  ")


(defun solve-part-1 (statements)
  "Solve part 1."
  (let ((monkeys (sort (mapcar #'car statements) #'string<))
        (held-items nil)
        (monkey-throw-rules nil))
    ;; Set up held-items, alist with monkey as key, held items list as value.
    ;; Set up monkey-throw-rules, alist with monkey as key, throw rules alist as value.
    (iter
      (for (monkey . monkey-statements) in statements)
      (for items = (car (assoc-v "holding-items" monkey-statements)))
      (for just-rules = (remove "holding-items" monkey-statements :key #'car :test #'string-equal))
      (push (cons monkey items) held-items)
      (push (cons monkey just-rules) monkey-throw-rules)
      )
    (format t "held-items: ~a~%" held-items)
    ;; (iter
    ;;   (for round from 1 to 20)
    ;;   (iter
    ;;     (for monkey in monkeys)
    ;;     (for holding-items = (get-items monkey held-items))
    ;;     (for throw-rules = (get-throw-rules monkey monkey-throw-rules))
    ;;     (format t "Round ~a, monkey ~a: holding ~a~%" round monkey holding-items)
    ;;     )
    ;;   (terpri)
    
    ;; )
    held-items
    )
  
  
  )


(defun solve-part-2 (statements)
  "Solve part 2."
  (declare (ignorable statements))
  "TBD")


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 11."
  (let* ((raw-statements (get-input #P"./input"))
         (statements (parse-statements raw-statements))
         (part-1 (solve-part-1 statements))
         (part-2 (solve-part-2 statements)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

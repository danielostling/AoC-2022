;;;; day-7.lisp

(in-package #:day-7)

(defun get-input (path)
  "Read puzzle input from `path`, return as list of lines."
  (uiop:read-file-lines path))


(defun parse-history-line (history-line)
  "Determine parts of `history-line`.
   Return as list (command arg item-type item-name item-size).

   Examples:
   (\"ls\" nil nil nil nil)
   (\"cd\" \"..\" nil nil nil)
   (nil nil \"dir\" \"a\" nil)
   (nil nil \"file\" \"fddw.bgw\" 169838)"
  (let ((line-parts (uiop:split-string history-line)))
    (cond ((and (string= (first line-parts) "$")
                (string= (second line-parts) "ls"))
           '("ls" nil nil nil nil))
          ((and (string= (first line-parts) "$")
                (string= (second line-parts) "cd"))
           (list "cd" (nth 2 line-parts) nil nil nil))
          ((string= (first line-parts) "dir")
           (list nil nil "dir" (second line-parts) nil))
          (t (list nil nil "file"
                   (second line-parts)
                   (parse-integer
                    (first line-parts)))))))


(defun add-or-create-file (elem key hash-table)
  "Add `elem` to value of `key` in `hash-table`, where value is a list.
   If `key` is missing from `hash-table`, add (`elem`), else add
   (`elem` previous-elements)"
  (multiple-value-bind (val present-p) (gethash key hash-table)
    (setf (gethash key hash-table)
          (cond ((null present-p) (list elem))
                (t (push elem val))))))


(defun add-or-create-dir (elem key hash-table)
  "Add (append '(`elem`) `key`) to `hash-table` with nil value.
   If `key` is present in `hash-table`, do nothing."
  (let ((new-key (append (list elem) key)))
    (multiple-value-bind (val present-p) (gethash new-key hash-table)
      (declare (ignorable val))
      (unless present-p
        (setf (gethash new-key hash-table) nil)))))


(defun reverse-keys (hash-table)
  "Reverse all keys in `hash-table` and return the new hash table."
  (let ((new-hash-table (make-hash-table :test #'equal)))
    (iter
      (for (key val) in-hashtable hash-table)
      (for new-key = (reverse key))
      (setf (gethash new-key new-hash-table) val))
    new-hash-table))


(defun hash-keys (hash-table)
  "Return list of keys in `hash-table`."
  (iter
    (for (key nil) in-hashtable hash-table)
    (collect key)))


(defun build-filesystem (input)
  "Use command history in `input` to build the file system."
  (let ((filesystem (make-hash-table :test #'equal))
        (current-dir '("/")))
    (iter
      (for history-line in input)
      (for (command arg item-type item-name item-size) =
           (parse-history-line history-line))
      (for linecount first 1 then (1+ linecount))
      (cond ((string= command "ls") (next-iteration))
            ((string= command "cd")
             (cond ((string= arg "..") (pop current-dir))
                   ((string= arg "/") (setf current-dir '("/")))
                   (t (push arg current-dir))))
            ((string= item-type "file")
             (progn
               (add-or-create-file
                (list item-name item-size)
                current-dir
                filesystem)))
            ((string= item-type "dir")
             (add-or-create-dir item-name current-dir filesystem))))
    filesystem))


(defun sum-directories-by-size-limit (limit summed-filesystem)
  "Return sum of sizes for directories whose recursive file size sum is
   `limit` or less in `summed-filesystem`."
  (let ((directories (hash-keys summed-filesystem)))
    (iter
      (for directory in directories)
      (for size = (gethash directory summed-filesystem))
      (when (<= size limit)
        (summing size)))))


(defun sum-directories (filesystem)
  "Return sum of sizes for directories whose recursive file size sum is
   `limit` or less in `filesystem`."
  (let ((directory-sizes (make-hash-table :test #'equal))
        (directories (hash-keys filesystem)))
    (iter
      (for (directory files) in-hashtable filesystem)
      (for directory-size = (reduce #'+ (mapcar #'second files)))
      (setf (gethash directory directory-sizes) directory-size))
    (iter
      (for directory in (sort (copy-seq directories)
                              #'(lambda (a b) (>= (length a) (length b)))))
      (for parent-directory = (butlast directory))
      (when (null parent-directory)
        (next-iteration))
      (for size = (gethash directory directory-sizes))
      (for parent-size = (gethash parent-directory directory-sizes))
      (setf (gethash parent-directory directory-sizes) (+ size parent-size)))
    directory-sizes))


(defun solve-part-1 (input)
  "Solve part 1."
  (let* ((filesystem (reverse-keys (build-filesystem input)))
         (summed-up-filesystem (sum-directories filesystem)))
    (sum-directories-by-size-limit 100000 summed-up-filesystem)))


(defun solve-part-2 (input)
  "Solve part 2."
  (let* ((total-disk-size 70000000)
         (upgrade-size 30000000)
         (filesystem (reverse-keys (build-filesystem input)))
         (summed-up-filesystem (sum-directories filesystem))
         (used-disk-size (gethash '("/") summed-up-filesystem))
         (free-disk-size (- total-disk-size used-disk-size))
         (to-delete-size (- upgrade-size free-disk-size))
         (directory-sizes (iter
                            (for (nil size) in-hashtable summed-up-filesystem)
                            (adjoining size :test #'=))))
    (iter
      (for size in (sort (copy-seq directory-sizes) #'<=))
      (when (>= size to-delete-size)
        (leave size)))))


(defun main ()
  "Solve part 1 and part 2 of AoC 2022 day 7."
  (let* ((raw-command-history (get-input #P"./input"))
         (part-1 (solve-part-1 raw-command-history))
         (part-2 (solve-part-2 raw-command-history)))
    (format t "First part: ~a~%" part-1)
    (format t "Second part: ~a~%" part-2)))

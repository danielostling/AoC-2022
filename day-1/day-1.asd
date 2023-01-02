;;;; day-1.asd

(asdf:defsystem #:day-1
  :description "Describe day-1 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:iterate)
  :serial t
  :components ((:file "package")
               (:file "day-1")))

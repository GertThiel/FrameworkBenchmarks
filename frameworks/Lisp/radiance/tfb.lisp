(in-package #:rad-user)
(define-module #:tfb
  (:use #:cl #:radiance))

(in-package #:tfb)


(define-page plaintext "/plaintext" ()
  (setf (content-type *response*) "text/plain")
  "Hello, World!")

(define-page json "/json" ()
  (setf (content-type *response*) "application/json")
  (jonathan:to-json '(:message "Hello, World!")))

(defun get-all-fortunes ()
  (postmodern:query (:select 'id 'message :from 'fortune) :rows))

(defun get-all-fortunes-plus-one ()
  (let* ((records       (get-all-fortunes))
         (records-p-one (append records '((0 "Additional fortune added at request time.")))))
    (sort (copy-list records-p-one) #'string-lessp :key #'second)))

(define-page fortunes "/fortunes" ()
  (setf (content-type *response*) "text/html; charset=UTF-8")
  (cl-markup:html5
    (:head
      (:title "Fortunes"))
    (:body
      (:table
        (:tr
          (:th "id")
          (:th "message"))
        (loop for fortune-row in (get-all-fortunes-plus-one)
              collect (cl-markup:markup
                        (:tr
                          (:td (format nil "~d" (first fortune-row)))
                          (:td (second fortune-row)))))))))

(define-page fortunes-clip "/fortunes-clip" (:clip "fortunes.ctml")
  (setf (content-type *response*) "text/html; charset=UTF-8")
  (r-clip:process t))

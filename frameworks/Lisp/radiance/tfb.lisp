(in-package #:rad-user)
(define-module #:tfb
  (:use #:cl #:radiance))

(in-package #:tfb)


(define-page plaintext "/plaintext" ()
  "Plaintext handler."
  (setf (content-type *response*) "text/plain")
  "Hello, World!")

(define-api json () ()
  "JSON handler using Jonathan to encode JSON"
  (setf (content-type *response*) "application/json")
  (jonathan:to-json '(:message "Hello, World!")))

(defun get-a-random-record (id)
  (declare (integer id))
  (i-postmodern::with-connection
    `(:|id| ,id :|randomNumber| ,(postmodern:query (:select 'randomnumber :from 'world :where (:= 'id id)) :single!))))

(define-page db "/db" ()
  "DB handler using Jonathan to encode JSON and Postmodern to access PostgreSQL"
  (let ((id (+ 1 (random 10000))))
    (jonathan:to-json (get-a-random-record id))))

(defun ensure-integer-is-between-one-and-five-hundreds (n)
  (declare (integer n))
  (if (< n 1)
    (values 1 nil)
    (if (> n 500)
      (values 500 nil)
      (values n t))))

(defun extract-number-of-records-to-fetch (n)
  (let ((n (handler-case
            (parse-integer n)
            (error (c) (values 1 c)))))
    (ensure-integer-is-between-one-and-five-hundreds n)))

(defun get-some-random-integers-between-one-and-ten-thousand (n)
  (declare (integer n))
  (loop :repeat n
        :collect (+ 1 (random 10000))))

(defun get-some-random-records (n)
  (declare (integer n))
  (let ((ids (get-some-random-integers-between-one-and-ten-thousand n)))
    (mapcar #'get-a-random-record ids)))

(define-api queries (&optional queries) ()
  "QUERIES handler using Jonathan to encode JSON and Postmodern to access PostgreSQL"
  (setf (content-type *response*) "application/json")
  (jonathan:to-json (get-some-random-records (extract-number-of-records-to-fetch queries))))

(defun get-all-fortunes ()
  (i-postmodern::with-connection
   (postmodern:query (:select 'id 'message :from 'fortune) :rows)))

(defun get-all-fortunes-plus-one ()
  (let* ((records       (get-all-fortunes))
         (records-p-one (append records '((0 "Additional fortune added at request time.")))))
    (sort (copy-list records-p-one) #'string-lessp :key #'second)))

(define-page fortunes "/fortunes" ()
  "FORTUNES handler using Jonathan to encode JSON, Postmodern to access PostgreSQL and CL-Markup to build the HTML"
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

;; (defun db-get-all-fortunes ()
;;   (db:query 'fortune :all))

;; (define-page fortunes-clip "/fortunes-clip" (:clip "fortunes.ctml")
;;   (setf (content-type *response*) "text/html; charset=UTF-8")
;;   (r-clip:process t ; :fortunes fortunes
;;                   ))

(defun get-and-update-some-random-records (n)
  (declare (integer n))
  (let* ((random-records (get-some-random-records n))
         (random-numbers (get-some-random-integers-between-one-and-ten-thousand n))
         (index -1)
         (updated-records (map 'list
                               (lambda (row)
                                       (incf index)
                                       (list :|id|           (getf row :|id|          )
                                             :|randomNumber| (nth index random-numbers)))
                               random-records))
         (record-list     (map 'list
                               (lambda (row)
                                       (list (nth 1 row)
                                             (nth 3 row)))
                               updated-records)))
    (i-postmodern::with-connection
      (postmodern:query (format nil "UPDATE world AS ori SET randomnumber = new.randomnumber FROM (VALUES ~{(~{~a~^, ~})~^, ~}) AS new (id, randomnumber) WHERE ori.id = new.id" record-list)))
    (values updated-records)))

(define-api updates (&optional queries) ()
  "UPDATES handler using Jonathan to encode JSON and Postmodern to access PostgreSQL"
  (setf (content-type *response*) "application/json")
  (jonathan:to-json (get-and-update-some-random-records (extract-number-of-records-to-fetch queries))))

(defpackage :st-utils
  (:use :common-lisp)
  (:export
   :with-gensyms
   :as-keyword
   :load-data
   :group
   :hash-string
   :mklist
   :save-data
   :parse-simple-sexp))

(in-package :st-utils)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun load-data (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun save-data (data filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       )
    (with-standard-io-syntax
      (print data out))))

(defun hash-string (string)
  "Hashing a string with md5 hashing algorithm a give back an hexadecimal string"
  (let ((id (with-output-to-string (s)
              (loop for var across (md5:md5sum-string string)
                    do (write-string (write-to-string var :base 16) s)))))
    (string-downcase id)))

(defun group (list n)
  "For grouping lists into sublists.
You give group a list l and a number n, and it will return a new list in which the elements of l are grouped 
into sublists of length n. The remainder is put in a final sublist.
Example:  (group ’(a b c d e f g) 2) => ((A B) (C D) (E F) (G))
This code is from the book OnLisp, chapter 4."
  (if (zerop n) (error "zero length"))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                   (rec rest
                        (cons (subseq list 0 n)
                              acc))
                   (nreverse (cons list acc))))))
    (if list (rec list nil) nil)))

(defun mklist (x) (if (listp x) x (list x)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (let ((car-of-form (car form)))
         (or (funcall test car-of-form)
             (and (cons-form-p car-of-form))))))

(defun make-instance-for (object-name value)
  (apply #'make-instance object-name value))

(defun parse-simple-sexp (sexp)
  "Parse an expression of the form (:tag :prop 1 :prop2 2 :prop3 3)
to produce an object of type tag
example: (parse-simple-sexp '(:identifier :pos 20 :end 34 :escaped-text \"log\"))"
  (destructuring-bind (tag &rest props) sexp
    (let ((object-name (find-symbol (symbol-name tag))))
      (make-instance-for object-name props))))

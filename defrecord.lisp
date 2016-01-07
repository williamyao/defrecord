;;;; Simpler record specification.

(defpackage #:defrecord
  (:use #:cl)
  (:nicknames #:record)
  (:export #:define
           #:copy-with
           #:copy-record))
(in-package #:defrecord)

(defclass record () ()
  (:documentation "Base type for all classes defined with DEFRECORD."))

(defmacro define (name (&rest superrecords) &body slots)
  "DEFRECORD -- more concise class definitions.

usage: (defrecord name (superrecord*) slot-specifier*)

NAME := a symbol
SUPERRECORD := superclasses, should also be defined using DEFRECORD
SLOT-SPECIFIER := slot-name | (slot-name default-value)

Defines:
* a by-order-of-arguments constructor based on the order that
  the slots were specified
* a shallow copy method on COPY-RECORD, including shallow copy
  of all superrecord slots"
  (destructuring-bind (slot-names default-values)
      (or (and slots (transpose (map 'list #'normalize-slot slots)))
          (list nil nil))  ; Handles the case of no slots gracefully.(
    (let* ((slot-names (map 'list #'string slot-names))
           (slot-symbols (map 'list #'intern slot-names))
           (slot-keywords (map 'list
                               #'(lambda (x) (intern x :keyword))
                               slot-names)))
      `(progn
         ,(record-defclass-form name
                                superrecords
                                slot-symbols
                                slot-keywords
                                default-values)
         ,(record-%copy-record-form name slot-symbols)
         ,(record-copy-record-form name)
         ,(record-boa-constructor-form name slot-symbols slot-keywords)
         ',name))))

(defun copy-with (record &rest symbols-and-values)
  "Return a copy of RECORD with the slots specified bound to 
different values."
  (let ((copy (copy-record record)))
    (loop for (symbol value) on symbols-and-values by #'cddr
          do (setf (slot-value copy symbol) value))
    copy))

(defgeneric copy-record (record)
  (:documentation "Create a shallow copy of the passed-in record, with the
same class."))
(defgeneric %copy-record (record instance)
  (:method-combination progn :most-specific-last))


(defun record-defclass-form (name
                             superrecords
                             slot-symbols
                             slot-keywords
                             default-values)
  `(defclass ,name ,(if superrecords superrecords '(record))
     ,(loop for slot-symbol in slot-symbols
            for slot-keyword in slot-keywords
            for default-value in default-values
            collect `(,slot-symbol
                      :reader ,slot-symbol
                      :initarg  ,slot-keyword
                      :initform ,default-value))))

(defun record-%copy-record-form (name slot-symbols)
  `(defmethod %copy-record progn ((record ,name) (instance ,name))
     ,@(loop for slot-symbol in slot-symbols
             collect `(setf (slot-value instance ',slot-symbol)
                            (slot-value record   ',slot-symbol)))))

(defun record-copy-record-form (name)
  `(defmethod copy-record ((record ,name))
     (let ((copy (make-instance ',name)))
       (%copy-record record copy)
       copy)))

(defun record-boa-constructor-form (name slot-symbols slot-keywords)
  `(defun ,name ,slot-symbols
     (make-instance ',name
                    ,@(alexandria:mappend
                       (lambda (symbol keyword) (list symbol keyword))
                       slot-keywords
                       slot-symbols))))

(defun transpose (list) (apply #'map 'list #'list list))
(defun normalize-slot (slot) (if (listp slot) slot (list slot nil)))

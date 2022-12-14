;;;; A rule consists of a predicate and an action.

(defclass rule ()
  ((predicate
    :initarg :predicate
    :initform (error "predicate must be provided")
    :reader predicate)
   (action
    :initarg :action
    :initform (error "action must be provided")
    :reader action)))

(defun random-bit-vector (length)
  (let ((vec (make-array length :element-type 'BIT :fill-pointer 0)))
    (dotimes (i length)
      (vector-push (random 2) vec))
    vec))

(defun evolve (pop-size address-bits)
  (let ((population (make-array pop-size :fill-pointer 0)))
    (dotimes (i pop-size)
      (let* ((data-bits (expt 2 address-bits))
             (predicate (random-bit-vector (+ address-bits data-bits)))
             (action (random 2))
             (rule (make-instance 'rule :predicate predicate :action action)))
        (vector-push rule population)))))

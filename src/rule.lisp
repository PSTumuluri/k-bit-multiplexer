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

(defun random-predicate (length)
  (let ((vec (make-array length :fill-pointer 0)))
    (dotimes (i length)
      (let ((x (random 3)))
        (vector-push (if (= x 2) '\# x) vec)))
    vec))

(defun print-population (population)
  (dotimes (i (length population))
    (let ((rule (elt population i)))
      (with-slots (predicate action) rule
        (format t "~a~t~a~%" (predicate rule) (action rule))))))

(defun initialize (pop-size address-bits)
  (let ((population (make-array pop-size :fill-pointer 0)))
    (dotimes (i pop-size)
      (let* ((data-bits (expt 2 address-bits))
             (predicate (random-predicate (+ address-bits data-bits)))
             (action (random 2))
             (rule (make-instance 'rule :predicate predicate :action action)))
        (vector-push rule population)))
    population))

(defun evolve (pop-size address-bits)
  (let ((population (initialize pop-size address-bits)))
    (print-population population)))

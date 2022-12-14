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

(defun random-action ()
  (random 2))

(defun print-rule (rule)
  (format t "~a : ~a" (predicate rule) (action rule)))

(defun random-rule (string)
  "Returns a random rule whose predicate matches the string and whose action is random."
  (let ((predicate (make-array (length string) :fill-pointer 0)))
    (dotimes (i (length string))
      ;; 50% chance to roll the existing bit, 50% chance to roll #.
      (let ((roll (random 1.0)))
        (vector-push (if (< roll 0.5) (elt string i) '\#) predicate)))
    (make-instance 'rule :predicate predicate :action (random-action))))

(defun print-population (population)
  (dotimes (i (length population))
    (print-rule (elt population i))))

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

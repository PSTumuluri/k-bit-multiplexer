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

(defun random-binary ()
  (random 2))

(defun print-rule (rule)
  (format t "~a : ~a~%" (predicate rule) (action rule)))

(defun random-rule (string)
  "Returns a random rule whose predicate matches the string and whose action is random."
  (let ((predicate (make-array (length string) :fill-pointer 0)))
    (dotimes (i (length string))
      ;; 50% chance to roll the existing bit, 50% chance to roll #.
      (let ((roll (random 1.0)))
        (vector-push (if (< roll 0.5) (elt string i) '\#) predicate)))
    (make-instance 'rule :predicate predicate :action (random-binary))))

(defun print-population (population)
  (dotimes (i (length population))
    (print-rule (elt population i))))

(defun random-bit-string (size)
  (let ((string (make-array size :fill-pointer 0)))
    (dotimes (i size)
      (vector-push (random-binary) string))
    string))

(defun matches (predicate posted-string)
  (dotimes (i (length predicate))
    (let ((predicate-symbol (elt predicate i))
          (posted-string-symbol (elt posted-string i)))
      (if (not (or (eql predicate-symbol '\#)
                   (= predicate-symbol posted-string-symbol)))
          (return-from matches NIL))))
  T)

(defun find-matches (population posted-string)
  (let ((match-set (make-array (length population) :fill-pointer 0)))
    (dotimes (i (length population))
      (let ((genotype (elt population i)))
        (if (matches (predicate genotype) posted-string)
            (vector-push genotype match-set))))
    match-set))

(defun evolve (num-bits)
  "Evolve a rule set for the k-bit multiplexer problem."
  (let ((population (make-array 10 :fill-pointer 0)))
    (dotimes (i 10)
      (let* ((posted-string (random-bit-string num-bits))
             (match-set (find-matches population posted-string)))
        (format t "Posted string: ~a~%" posted-string)
        (when (= 0 (length match-set))
          (vector-push (random-rule posted-string) population))
        (format t "Match set:~%")
        (print-population match-set))
        (format t "Population: ~%")
        (print-population population)))))

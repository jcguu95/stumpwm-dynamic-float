;;;; gap.lisp

(in-package :stumpwm-dfg-II)

;;;

(defparameter *default-gap-size* 5)
(defparameter *default-gap-step* 5)

;;; OOP Model

(defclass gap-info ()
  ((gap-size :initform *default-gap-size* :accessor gap-size
    :documentation "The gap size between windows.")
   (gap-step :initform *default-gap-step* :accessor gap-step
    :documentation "The step size taken during gap alternation.")
   (gap-effective-p :initform t :accessor gap-effective-p
    :documentation "Whether gapping is effective: NON-NIL means
    effective and NIL means ineffective.")))

(defun gap-set (n &optional (gap? t) (group (current-group)))
  (assert (integerp n) () "Expected ~A (N) to be of type INTEGER." n)
  (assert (dyn-float-group-II-p group) () "Expected GROUP ~A to be of type DYN-FLOAT-II-GROUP." group)
  (setf (gap-size (gap-info group)) n
        (gap-effective-p (gap-info group)) gap?)
  (sync! group))

(defmacro defcommand-gap (options)
  (loop for option in options
        do (eval (let ((body (case option
                               ('toggle `(gap-set gap-size (not gap?) group))
                               ('set-default `(gap-set *default-gap-size* t group))
                               ('increase `(gap-set (+ gap-size gap-step) t group))
                               ('decrease `(gap-set (- gap-size gap-step) t group)))))
                   `(defcommand ,(read-from-string (concat "gap-" (format nil "~s" option)))
                        (&optional (group (current-group))) ()
                      (let ((gap-size (gap-size (gap-info group)))
                            (gap-step (gap-step (gap-info group)))
                            (gap? (gap-effective-p (gap-info group))))
                        ,body))))))

;; New Commands: gap-set-default, gap-increase, gap-decrease.
(defcommand-gap (toggle set-default increase decrease))

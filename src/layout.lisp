;;;; layout.lisp

(in-package :stumpwm-dfg-II)

;;;

(defvar +supported-layouts+
  '(;; right-vertical fibonacci deck vertical-roller horizontal-roller
    left-vertical fullscreen horizontal))
(defparameter *default-layout* (car +supported-layouts+))

;;; Layout Definition

;; TODO Remove the following after testing.
;; (defun head-mode-line-height (&optional (head (current-head)))
;;   (let* ((modeline (stumpwm::head-mode-line head)))
;;     (if modeline (stumpwm::mode-line-height modeline) 0)))

;; TODO Remove the following after testing.
;; (defun head-mode-line-position (&optional (head (current-head)))
;;   (let* ((modeline (stumpwm::head-mode-line head)))
;;     (if modeline (stumpwm::mode-line-position modeline) nil)))

(defmacro deflayout (name docstring body)
  "The macro for STUMPWM-DFG-II for defining layout. To learn the
DSL, consult the examples or read the slots in LET* form. The
defined layouts should return a plist (:x x :y y :width width
:height height) that describes the geometry of the kth tiled
windows among the N of them."
  `(defun ,name (_th amount-of-the-tiled
                 &optional
                   (group (current-group))
                   (screen (current-screen))
                   (head (current-head)))
     ,docstring
     (let* ((ml (stumpwm::head-mode-line head))
            (mlp (if ml (stumpwm::mode-line-position ml) nil))
            (mlh (if ml (stumpwm::mode-line-height ml) 0))
            (sw (- (screen-width screen)
                   (* 2 stumpwm::*float-window-border*)))
            (sh (- (screen-height screen)
                   (* 2 stumpwm::*float-window-border*)
                   mlh))
            (x0 0)
            (y0 (if (eq :top mlp) mlh 0))
            (mr (master-ratio group))
            (gap? (gap-effective-p (gap-info (current-group))))
            (gap (if gap? (gap-size (gap-info group)) 0))
            (k _th)
            (N amount-of-the-tiled))
       ,body)))

(deflayout fullscreen
  "A layout where every tiled windows are fully expanded."
  (list :x x0 :y y0 :width sw :height sh))

(deflayout left-vertical
  "A layout where the master is put on the left."
  (cond ((= 1 N) (list :x x0 :y y0 :width sw :height sh))
        ((< 1 N) (cond ((= k 0) (list :x (+ x0 gap) :width (- (round (* sw mr)) (* 2 gap))
                                      :y (+ y0 gap) :height (- sh (* 2 gap))))
                       ((> k 0) (list :x (+ x0 gap (round (* sw mr)))
                                      :y (+ y0 gap (* (round (/ sh (- N 1))) (- k 1)))
                                      :width (- (round (* sw (- 1 mr))) (* 2 gap))
                                      :height (- (round (/ sh (- N 1))) (* 2 gap))))))))

(deflayout horizontal
  "A layout where the master is put on the top."
  (cond ((= 1 N) (list :x x0 :y y0 :width sw :height sh))
        ((< 1 N) (cond ((= k 0) (list :x (+ x0 gap) :width (- sw (* 2 gap))
                                      :y (+ y0 gap) :height (- (round (* sh mr)) (* 2 gap))))
                       ((> k 0) (list :x (+ x0 gap (* (round (/ sw (- N 1))) (- k 1)))
                                      :y (+ y0 gap (round (* sh mr)))
                                      :width (- (round (/ sw (- N 1))) (* 2 gap))
                                      :height (- (round (* sh (- 1 mr))) (* 2 gap))))))))

;;; Master Ratio

(defmacro defcommand-master-ratio (options)
  `(progn
     ,@(loop for option in options
             collect (let ((body (case option
                                   ('set-default `(setf master-ratio *default-master-ratio*))
                                   ('increase `(setf master-ratio (* 1.05 master-ratio)))
                                   ('decrease `(setf master-ratio (* (/ 1 1.05) master-ratio))))))
                       `(defcommand ,(read-from-string (concat "master-ratio-" (format nil "~s" option)))
                            (&optional (group (current-group))) ()
                          (symbol-macrolet ((master-ratio (master-ratio group)))
                            ,body
                            (sync! group)))))))
;; New Commands:
;; + master-ratio-set-default
;; + master-ratio-increase
;; + master-ratio-decrease
(defcommand-master-ratio (set-default increase decrease))

;;; Commands

(defun layout-current (&optional (group (current-group)))
  (nth 0 (layout-hist group)))

(defun layout-change (layout &optional (group (current-group)))
  (if (member layout +supported-layouts+)
      (progn
        (setf (layout-hist group) (cons layout (layout-hist group)))
        (sync! group))
      (error "Layout ~s is not supported. See ~s. " layout '+supported-layouts+)))

(defcommand layout-change-next (&optional (group (current-group))) ()
  (let* ((sl +supported-layouts+)
         (np (+ 1 (position (layout-current group) sl)))
         (layout (if (>= np (length +supported-layouts+))
                     (car sl)
                     (nth np sl))))
    (layout-change layout group)
    (stumpwm::echo (format nil "LAYOUT: ~a" layout))))

(defcommand layout-toggle-fullscreen (&optional (group (current-group))) ()
  (if (eq 'fullscreen (layout-current group))
      (layout-change (find-if (lambda (x)
                                (not (eq x 'fullscreen)))
                              (layout-hist group))
                     group)
      (layout-change 'fullscreen group)))

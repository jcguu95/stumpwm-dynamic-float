;;;; main.lisp

(in-package :stumpwm-dfg-II)

;;;

(defvar +status-list+ '(background tiled foreground))
(defvar +1-golden-ratio+ (/ 2 (+ 1 (sqrt 5))))
(defparameter *default-master-ratio* +1-golden-ratio+)

;;; OOP Model

(defclass dyn-float-group-II (stumpwm::float-group)
  ((dfg-data :initform nil :accessor dfg-data
             :documentation "The main list of dfg-datum used to
             hold the information for redrawing and renumbering
             the windows internal to STUMPWM.")
   (layout-hist :initform (list *default-layout*) :accessor layout-hist
                :documentation "The list of layout
                histories,where the zeroth element is interpreted
                as the current layout.")
   (master-ratio :initform *default-master-ratio* :accessor master-ratio
                 :documentation "Ratio of the master window takes.")
   (gap-info :initform (make-instance 'gap-info) :accessor gap-info
             :documentation "Info for gapping.")))

(defclass dfg-datum ()
  ((xwin-id :initarg :dfg-datum-xwin-id :accessor dfg-datum-xwin-id)
   (x :initarg :dfg-datum-x :accessor dfg-datum-x)
   (y :initarg :dfg-datum-y :accessor dfg-datum-y)
   (height :initarg :dfg-datum-height :accessor dfg-datum-height)
   (width :initarg :dfg-datum-width :accessor dfg-datum-width)
   (window-number :initarg :dfg-datum-window-number :accessor dfg-datum-window-number)
   (status :initarg :dfg-datum-status :accessor dfg-datum-status
           :documentation "This slot is special to
           stumpwm-dfg-II. Its possible values are recorded in
           *status-list*."))
  (:documentation "The datum for redrawing and renumbering the
  windows internal to STUMPWM."))

;;; Misc.

(defparameter *new-window-spec-hooks* nil
  "Hooks for specifying spec for new windows satisfying
customizable predicates. For each new window, the hooks are
called one by one by #'NEW-WINDOW-SPEC until it succeeds. For
example, when its value is

    (list (cons (lambda (w) (equal 0 (search \"[TOP]\" (window-title w))))
                (list :x 300 :y 300 :height 300 :width 300 :status 'foreground)))

Then all new windows that satisfy the predicate (namely, those
whose titles start with \"[TOP]\") will be assigned to the
spec.")

(defun new-window-spec (window)
  "Generate spec for new windows WINDOW based on the customizable
hooks in *NEW-WINDOW-SPEC-HOOKS*. It calls the hooks one by one,
and breaks the loop once a predicate is satisfied. NIL is
returned if no hook succeeds."
  (block outer
    (loop for hook in *new-window-spec-hooks*
          do (when (funcall (car hook) window)
               (return-from outer (cdr hook))))))

(defun override-internal-with-dfg-datum (window)
  "If WINDOW has a nontrivial piece of datum, overwrite the
values (x, y, width, height) into the slots of WINDOW internal to
StumpWM."
  (let ((datum (get-dfg-datum window)))
    (when datum
      (with-accessors ((x window-x) (width window-width)
                       (y window-y) (height window-height))
          window
        (setf x (dfg-datum-x datum) width (dfg-datum-width datum)
              y (dfg-datum-y datum) height (dfg-datum-height datum))))))

(defmethod make-dfg-datum ((window stumpwm::float-window))
  "It should only be called when a new window is created."
  (flet ((unless- (x y) (if x x y)))
    (let* ((spec   (new-window-spec window))
           (x      (unless- (getf spec :x)      (window-x window)))
           (y      (unless- (getf spec :y)      (window-y window)))
           (width  (unless- (getf spec :width)  (window-width window)))
           (height (unless- (getf spec :height) (window-height window)))
           (status (unless- (getf spec :status) 'tiled)))
      (make-instance 'dfg-datum :dfg-datum-status status
                                :dfg-datum-x x :dfg-datum-width width
                                :dfg-datum-y y :dfg-datum-height height
                                :dfg-datum-xwin-id (get-xwin-id window)
                                :dfg-datum-window-number (window-number window)))))

(defmethod get-dfg-datum ((w stumpwm::float-window)
                          &optional (group (current-group)))
  (find-if (lambda (datum)
             (equal (get-xwin-id w)
                    (dfg-datum-xwin-id datum)))
           (dfg-data group)))

(defun dyn-float-group-II-p (group)
  "The predicate of dyn-float-group-II."
  (eq (type-of group) 'dyn-float-group-II))

(defun tiled? (dfg-datum) (eq 'tiled (dfg-datum-status dfg-datum)))

(defmethod get-xwin-id ((w stumpwm::float-window))
  (slot-value (window-xwin w) 'xlib::id))

(defun get-xwin-with-xwin-id (xwin-id &optional (group (current-group)))
  (find-if (lambda (window) (= xwin-id (get-xwin-id window)))
           (stumpwm::group-windows group)))

(defun count-tiled (&optional (group (current-group)))
  (length (remove-if-not
           (lambda (dfg-datum)
             (equal 'tiled (dfg-datum-status dfg-datum)))
           (dfg-data group))))

;;; Core

(defun sync! (&optional (group (current-group)))
  "Main function of STUMPWM-DFG-II."
  (sync-existence group)
  (update-dfg-data group)
  (apply-data! group))

(defun sync-existence (&optional (group (current-group)))
  "Sync the existence data between the window list (internal to
STUMPWM) and the DFG-DATA of the GROUP in the following (ordered)
manner:

  1. For each window in the internal list, ensure the existence
  of its datum in the DFG-DATUM.

  2. Remove each datum in the DFG-DATUM that does not correspond
  to a window in the internal list."
  (symbol-macrolet ((dfg-data (dfg-data group)))
    (flet ((ensure-existence (window)
             (unless (member (get-xwin-id window)
                             (mapcar #'dfg-datum-xwin-id dfg-data))
               (setf dfg-data (append dfg-data (list (make-dfg-datum window))))
               (override-internal-with-dfg-datum window))))
      (let* ((internal (stumpwm::group-windows group))
             (internal-ids (mapcar #'get-xwin-id internal)))
        (loop for window in internal
              do (ensure-existence window))
        (loop for datum in dfg-data
              if (not (member (dfg-datum-xwin-id datum) internal-ids))
                do (setf dfg-data (remove datum dfg-data)))))))

(defun update-dfg-data (&optional (group (current-group)))
  "Calculate and update the values (x, y, width, height,
window-number) in the DFG-DATA of the GROUP."
  (symbol-macrolet ((dfg-data (dfg-data group)))
    ;; Sort the list DFG-DATA upon the rule: background < tiled < foreground.
    (setf dfg-data (stable-sort dfg-data (sort-logic +status-list+ :transformer #'dfg-datum-status)))
    (loop for k below (length dfg-data)
          do (progn
               ;; Renumber the window-number based on the actual location.
               (setf (dfg-datum-window-number (nth k dfg-data)) k)
               (with-accessors ((ax dfg-datum-x) (aw dfg-datum-width)
                                (ay dfg-datum-y) (ah dfg-datum-height))
                   (nth k dfg-data)
                 ;; Update the values (x, y, width, height) according to the internal window lists.
                 (let ((xwin (get-xwin-with-xwin-id (dfg-datum-xwin-id (nth k dfg-data)) group)))
                   (setf ax (window-x xwin) aw (window-width xwin)
                         ay (window-y xwin) ah (window-height xwin)))
                 ;; Update the dimensions for the dfg-data with status being tiled.
                 (when (tiled? (nth k dfg-data))
                   (let ((xywh (funcall (layout-current group)
                                        (location (nth k dfg-data) dfg-data :such-that #'tiled?)
                                        (count-tiled group))))
                     (setf ax (getf xywh :x) aw (getf xywh :width)
                           ay (getf xywh :y) ah (getf xywh :height)))))))))

(defun apply-data! (&optional (group (current-group)))
  "Renumber and redraw the windows in the internal list using the
data in DFG-DATA of the GROUP. Warning: Calling this directly may
crashes STUMPWM. It should be called after #'sync-existence."
  (loop for datum in (dfg-data group)
        do (with-accessors ((x dfg-datum-x) (width dfg-datum-width)
                            (y dfg-datum-y) (height dfg-datum-height)
                            (xwin-id dfg-datum-xwin-id)
                            (win-number dfg-datum-window-number))
               datum
             (let ((window (get-xwin-with-xwin-id xwin-id group)))
               (setf (window-number window) win-number) ; renumber!
               (stumpwm::float-window-move-resize       ; redraw!
                window :x x :y y :width width :height height)))))

;;; Interaction Methods

(flet ((add-float-window (group window raise)
         (change-class window 'stumpwm::float-window)
         (stumpwm::float-window-align window)
         (stumpwm::group-focus-window group window)))
  (defmethod group-add-window
      ((group dyn-float-group-II) window &key raise &allow-other-keys)
    (add-float-window group window raise)
    ;; (call-next-method) ; commented because of an undesired
    ;;                    ; change in commit: b6529119559231c9b60e4c7052946afb2cb454a7
    (sync! group)))

(defmethod group-delete-window
    ((group dyn-float-group-II) (window stumpwm::float-window))
  (call-next-method)
  (sync! group))

(defmethod group-button-press
    ((group dyn-float-group-II) button x y (window stumpwm::float-window))
  "Free the window if it's pressed at the boarder or with
*float-window-modifier*."
  (let ((xwin (stumpwm:window-xwin window)))
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer (stumpwm::window-parent window))
      (declare (ignore relx rely same-screen-p child))
      (when (or (< x (xlib:drawable-x xwin))
                (> x (+ (xlib:drawable-width xwin)
                        (xlib:drawable-x xwin)))
                (< y (xlib:drawable-y xwin))
                (> y (+ (xlib:drawable-height xwin)
                        (xlib:drawable-y xwin)))
                (intersection (stumpwm::float-window-modifier)
                              (xlib:make-state-keys state-mask)))
        (window-status-change 'foreground window group))))
  (call-next-method))

;;; Window Status

(defun window-status-change (status &optional (window (current-window)) (group (current-group)))
  (assert (dyn-float-group-II-p group) () "Expected GROUP ~A to be of type DYN-FLOAT-GROUP-II." group)
  (setf (dfg-datum-status (get-dfg-datum window)) status)
  (sync! group))

(defmacro defcommand-window-status (options)
  `(progn
     ,@(loop for option in options
             collect `(defcommand ,(read-from-string
                                    (concat "window-status-make-"
                                            (format nil "~s" option)))
                          (&optional (window (current-window)) (group (current-group))) ()
                        (window-status-change (quote ,option) window group)))))
;; New Commands:
;; + window-status-make-foreground
;; + window-status-make-tiled
;; + window-status-make-background
(defcommand-window-status (foreground tiled background))

(defcommand window-status-tile-at-point (&optional (window (current-window))
                                         (group (current-group))) ()
  (window-status-make-tiled window group))

(defcommand window-status-make-all-tiled (&optional (group (current-group))) ()
  (loop for w in (group-windows group)
        do (window-status-make-tiled w group)))

;;; Group Adding

(defcommand gnew-dyn-float-II (name &optional background) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME."
  (unless name (throw 'error :abort))
  (add-group (stumpwm:current-screen) name
             :type 'dyn-float-group-II :background background))

(defcommand gnew-dyn-float-II-bg (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME in the
background."
  (gnew-dyn-float-II name t))

(in-package :stumpwm-dfg)

;;;

(defmacro defcommand-window-focus (options)
  `(progn
     ,@(loop for option in options
             collect `(defcommand ,(read-from-string (concat "window-focus-" (format nil "~s" option)))
                          (&optional (group (current-group))) ()
                        (,(read-from-string (concat "stumpwm::focus-" (format nil "~s" option) "-window"))
                         group)))))
;; Commands: window-focus-next, window-focus-prev.
(defcommand-window-focus (next prev))

(defun permute-tiled-at-point-with-next (&optional (group (current-group)) &key reverse)
  (let* ((cw (group-current-window group))
         (dfg-data (dfg-data group)))
    (if (tiled? (get-dfg-datum cw))
        (progn
          (setf (dfg-data group)
                (append (remove-if #'tiled? (dfg-data group))
                        (permute-with-next (get-dfg-datum cw)
                                           (remove-if-not #'tiled? (dfg-data group))
                                           :reverse reverse)))
          (sync! group))
        (error "Current window must be tiled to be permuted."))))

(defcommand permute-window (&optional (group (current-group))) ()
  (permute-tiled-at-point-with-next group))
(defcommand permute-window-reverse (&optional (group (current-group))) ()
  (permute-tiled-at-point-with-next group :reverse t))

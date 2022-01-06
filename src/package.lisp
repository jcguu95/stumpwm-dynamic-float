(defpackage stumpwm-dfg
  (:use #:cl #:stumpwm)
  (:export #:master-ratio-increase
           #:master-ratio-decrease
           #:master-ratio-set-default

           #:window-status-tile-at-point
           #:window-status-make-all-tiled

           ;; Navigation
           #:window-focus-next
           #:window-focus-prev
           #:permute-window
           #:permute-window-reverse

           ;; Layout
           #:deflayout                  ; macro
           #:+supported-layouts+
           #:layout-toggle-fullscreen
           #:layout-change-next

           ;; Gap
           #:gap-toggle
           #:gap-set-default
           #:gap-increase
           #:gap-decrease
           #:*default-gap-size*
           #:*default-gap-step*

           ;; Add
           #:gnew-dyn-float
           #:gnew-dyn-float-bg

           #:*new-window-spec-hooks*
           ))

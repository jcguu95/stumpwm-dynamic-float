(defsystem :stumpwm-dynamic-float
  :name "stumpwm-dynamic-float"
  :description "stumpwm-dynamic-float is an extension to the X
  window manager \"StumpWM\". It provides a dynamic-tiling
  environment based on StumpWM's floating-group."
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "1.0.0"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "util")
                             (:file "main")
                             (:file "gap")
                             (:file "layout")
                             (:file "navigation")
                             )))
  :serial t
  :depends-on (:stumpwm))

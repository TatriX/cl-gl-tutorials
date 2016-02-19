(asdf:defsystem #:gl-tutorials
  :serial t
  :description "gl examples"
  :author "TatriX <tatrics@gmail.com>"
  ;; :license "MIT"

  :depends-on (:alexandria :sdl2kit :defpackage-plus :glkit :mathkit)
  ;; :pathname "examples"
  :serial t

  :components
  ((:file "package")
   (:file "01-hello/hello")
   (:file "02-colors/colors")
   (:file "03-moving-triangle/cpu-position-offset")
   (:file "03-moving-triangle/vertex-position-offset")
   (:file "03-moving-triangle/vertex-calc-offset")))

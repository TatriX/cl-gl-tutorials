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
   (:file "main")
   (:file "01/hello")
   (:file "02/colors")
   (:file "03/cpu-position-offset")
   (:file "03/vertex-position-offset")
   (:file "03/vertex-calc-offset")
   (:file "03/fragment-change-color")
   (:file "04/ortho-cube")
   (:file "04/shader-perspective")
   (:file "04/matrix-perspective")
   (:file "05/overlap-no-depth")
   (:file "05/base-vertex-overlap")))

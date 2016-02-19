(in-package :defpackage+-user-1)

;; KIT.SDL2.TEST

(defpackage+ :gl-tutorial.hello
  (:use #:cl #:alexandria #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.colors
  (:use #:cl #:alexandria #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))


(defpackage+ :gl-tutorial.3.cpu-position-offset
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))


(defpackage+ :gl-tutorial.3.vertex-position-offset
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.3.vertex-calc-offset
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

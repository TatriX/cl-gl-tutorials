(in-package :defpackage+-user-1)

;; KIT.SDL2.TEST

(defpackage+ :gl-tutorial.main
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main
           :main-window
           :uniform
           :*programs-dict*))

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

(defpackage+ :gl-tutorial.3.fragment-change-color
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.4.orhto-cube
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.4.shader-perspective
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.4.matrix-perspective
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export :main))

(defpackage+ :gl-tutorial.5.overlap-no-depth
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math #:gl-tutorial.main)
  (:export :main))

(defpackage+ :gl-tutorial.5.base-vertex-overlap
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math #:gl-tutorial.main)
  (:export :main))

(defpackage+ :gl-tutorial.5.depth-buffer
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math #:gl-tutorial.main)
  (:export :main))

(defpackage+ :gl-tutorial.5.vertex-clipping
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math #:gl-tutorial.main)
  (:export :main))

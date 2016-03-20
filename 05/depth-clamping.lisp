(in-package :gl-tutorial.5.depth-clamping)

(defconstant +num-of-vertices+ 36)

(let* ((right-extent 0.8)
       (left-extent (- right-extent))
       (top-extent 0.2)
       (middle-extent 0.0)
       (bottom-extent (- top-extent))
       (front-extent -1.25)
       (rear-extent -1.75)

       (green-color '(0.75 0.75 1.0 1.0))
       (blue-color '(0.0 0.5 0.0 1.0))
       (red-color '(1.0 0.0 0.0 1.0))
       (grey-color '(0.8 0.8 0.8 1.0))
       (brown-color '(0.5 0.5 0.0 1.0))
       (data (alexandria:flatten (list left-extent top-extent  rear-extent
                                       left-extent middle-extent  front-extent
                                       right-extent middle-extent  front-extent
                                       right-extent top-extent  rear-extent

                                       left-extent bottom-extent  rear-extent
                                       left-extent middle-extent  front-extent
                                       right-extent middle-extent  front-extent
                                       right-extent bottom-extent  rear-extent

                                       left-extent top-extent  rear-extent
                                       left-extent middle-extent  front-extent
                                       left-extent bottom-extent  rear-extent

                                       right-extent top-extent  rear-extent
                                       right-extent middle-extent  front-extent
                                       right-extent bottom-extent  rear-extent

                                       left-extent bottom-extent  rear-extent
                                       left-extent top-extent  rear-extent
                                       right-extent top-extent  rear-extent
                                       right-extent bottom-extent  rear-extent

                                       ;; Object 2 positions
                                       top-extent  right-extent rear-extent
                                       middle-extent right-extent front-extent
                                       middle-extent left-extent front-extent
                                       top-extent  left-extent rear-extent

                                       bottom-extent right-extent rear-extent
                                       middle-extent right-extent front-extent
                                       middle-extent left-extent front-extent
                                       bottom-extent left-extent rear-extent

                                       top-extent  right-extent rear-extent
                                       middle-extent right-extent front-extent
                                       bottom-extent right-extent rear-extent

                                       top-extent  left-extent rear-extent
                                       middle-extent left-extent front-extent
                                       bottom-extent left-extent rear-extent

                                       bottom-extent right-extent rear-extent
                                       top-extent  right-extent rear-extent
                                       top-extent  left-extent rear-extent
                                       bottom-extent left-extent rear-extent

                                       ;; Object 1 colors
                                       green-color
                                       green-color
                                       green-color
                                       green-color

                                       blue-color
                                       blue-color
                                       blue-color
                                       blue-color

                                       red-color
                                       red-color
                                       red-color

                                       grey-color
                                       grey-color
                                       grey-color

                                       brown-color
                                       brown-color
                                       brown-color
                                       brown-color

                                       ;; Object 2 colors
                                       red-color
                                       red-color
                                       red-color
                                       red-color

                                       brown-color
                                       brown-color
                                       brown-color
                                       brown-color

                                       blue-color
                                       blue-color
                                       blue-color

                                       green-color
                                       green-color
                                       green-color

                                       grey-color
                                       grey-color
                                       grey-color
                                       grey-color))))
  (defparameter *vertex-data* (cffi:foreign-alloc
                               :float
                               :initial-contents data))
  (defparameter *vertex-data-size* (* (length data) (cffi:foreign-type-size :float))))


(let ((data (list 0 2 1
                  3 2 0

                  4 5 6
                  6 7 4

                  8 9 10
                  11 13 12

                  14 16 15
                  17 16 14)))
  (defparameter *index-data* (cffi:foreign-alloc
                              :short
                              :initial-contents data))
  (defparameter *index-data-size* (* (length data) (cffi:foreign-type-size :short))))

(defun initialize-program ()
  (setf *programs-dict* (load-shaders))

  (use-program *programs-dict* :program)

  (let ((frustum-scale 1.0)
        (z-near 1.0)
        (z-far 3.0))
    (uniform :mat :perspective-matrix
             (vector
              (sb-cga:matrix frustum-scale 0.0 0.0 0.0
                             0.0 frustum-scale 0.0 0.0
                             0.0 0.0 (/ (+ z-near z-far) (- z-near z-far)) (/ (* 2 z-near z-far) (- z-near z-far))
                             0.0 0.0 -1.0 0.0))))
  (gl:use-program 0))

(defun load-shaders ()
  (defdict shaders (:shader-path
                    (merge-pathnames
                     #p "05/shaders/" (asdf/system:system-source-directory :gl-tutorials)))
    (shader standard-v :vertex-shader (:file "standard.vert"))
    (shader standard-f :fragment-shader (:file "standard.frag"))
    (program :program (:offset :perspective-matrix) ; uniforms
             (:vertex-shader standard-v)
             (:fragment-shader standard-f)))
  (compile-shader-dictionary 'shaders))

(defvar *vertex-buffer-object*)
(defvar *index-buffer-object*)

(defun initialize-vertex-buffer ()
  (setf *vertex-buffer-object* (gl:gen-buffer))

  (gl:bind-buffer :array-buffer *vertex-buffer-object*)
  (%gl:buffer-data :array-buffer *vertex-data-size* *vertex-data* :static-draw)
  (gl:bind-buffer :array-buffer 0)

  (setf *index-buffer-object* (gl:gen-buffer))

  (gl:bind-buffer :element-array-buffer *index-buffer-object*)
  (%gl:buffer-data :element-array-buffer *index-data-size* *index-data* :static-draw)
  (gl:bind-buffer :element-array-buffer 0))

(defvar *vao*)

(defmethod initialize-instance :after ((w main-window) &key &allow-other-keys)
  (setf (idle-render w) t)
  (gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer-bit)

  (gl:viewport 0 0 800 600)

  (initialize-program)
  (initialize-vertex-buffer)

  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)

  (let ((color-data-offset (* (cffi:foreign-type-size :float) 3 +num-of-vertices+)))
    (gl:bind-buffer :array-buffer *vertex-buffer-object*)
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    (gl:vertex-attrib-pointer 1 4 :float :false 0 color-data-offset)
    (gl:bind-buffer :element-array-buffer *index-buffer-object*))

  (gl:bind-vertex-array 0)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0 1))

(defparameter *depth-clamping-active* nil)

(defmethod keyboard-event :after ((window gl-window) state ts repeat-p keysym)
  (when (eql state :keydown)
    (let ((scancode (sdl2:scancode keysym)))
      (case scancode
        (:scancode-return (if *depth-clamping-active*
                              (gl:disable :depth-clamp)
                              (gl:enable :depth-clamp))
                          (setf *depth-clamping-active* (not *depth-clamping-active*)))))))

(defmethod render ((window main-window))
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (use-program *programs-dict* :program)


  (gl:bind-vertex-array *vao*)

  (uniform :vec :offset (vec3 0.0 0.0 0.5))

  (%gl:draw-elements :triangles
                     (/ *index-data-size* (cffi:foreign-type-size :short))
                     :unsigned-short
                     0)

  (uniform :vec :offset (vec3 0.0 0.0 -1.0))
  (%gl:draw-elements-base-vertex :triangles
                                 (/ *index-data-size* (cffi:foreign-type-size :short))
                                 :unsigned-short
                                 0
                                 (/ +num-of-vertices+ 2))

  (gl:bind-vertex-array 0)
  (gl:use-program 0))

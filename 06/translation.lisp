(in-package :gl-tutorial.6.translation)

(defun calc-frustum-scale (fov-deg)
  (/ 1
     (tan (/ (deg-to-rad fov-deg)
             2))))

(defmacro with-matrix (mat &body body)
  `(let ((,mat (zero-matrix))
         (x 0)
         (y 1)
         (z 2)
         (w 3))
     ,@body
     ,mat))

(defun initialize-program ()
  (setf *programs-dict* (load-shaders))

  (use-program *programs-dict* :program)

  (uniform :mat :camera-to-clip-matrix
           (let ((frustum-scale 1.0)
                 (z-near 1.0)
                 (z-far 45.0))
             (vector
              (with-matrix mat
                (setf (mref mat 0 x) frustum-scale
                      (mref mat 1 y) frustum-scale
                      (mref mat 2 z) (/ (+ z-near z-far)
                                        (- z-near z-far))
                      (mref mat 2 w) -1.0
                      (mref mat 3 z) (/ (* 2 z-near z-far) (- z-near z-far)))))))
  (gl:use-program 0))

(defun load-shaders ()
  (defdict shaders (:shader-path
                    (merge-pathnames
                     #p "06/shaders/" (asdf/system:system-source-directory :gl-tutorials)))
    (shader standard-v :vertex-shader (:file "pos-color-local-transform.vert"))
    (shader standard-f :fragment-shader (:file "color-passthrough.frag"))
    (program :program (:model-to-camera-matrix :camera-to-clip-matrix) ; uniforms
             (:vertex-shader standard-v)
             (:fragment-shader standard-f)))
  (compile-shader-dictionary 'shaders))

(defconstant +num-of-vertices+ 8)

(let* ((green-color '(0.75 0.75 1.0 1.0))
       (blue-color '(0.0 0.5 0.0 1.0))
       (red-color '(1.0 0.0 0.0 1.0))
       (brown-color '(0.5 0.5 0.0 1.0))
       (data (alexandria:flatten (list
                                  +1.0 +1.0 +1.0
                                  -1.0 -1.0 +1.0
                                  -1.0 +1.0 -1.0
                                  +1.0 -1.0 -1.0

                                  -1.0 -1.0 -1.0
                                  +1.0 +1.0 -1.0
                                  +1.0 -1.0 +1.0
                                  -1.0 +1.0 +1.0

                                  green-color
                                  blue-color
                                  red-color
                                  brown-color

                                  green-color
                                  blue-color
                                  red-color
                                  brown-color))))
  (defparameter *vertex-data* (cffi:foreign-alloc
                               :float
                               :initial-contents data))
  (defparameter *vertex-data-size* (* (length data) (cffi:foreign-type-size :float))))


(let ((data (list
             0 1 2
             1 0 3
             2 3 0
             3 2 1

             5 4 6
             4 5 7
             7 6 4
             6 7 5)))
  (defparameter *index-data* (cffi:foreign-alloc
                              :short
                              :initial-contents data))
  (defparameter *index-data-size* (* (length data) (cffi:foreign-type-size :short))))


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

(defun stationary-offset (ellapsed-time)
  (declare (ignore ellapsed-time))
  (list 0 0 -20))

(defun oval-offset (ellapsed-time)
  (let* ((loop-duration 3)
         (scale (/ (* 2 pi) loop-duration))
         (cur-time-throug-loop (mod ellapsed-time loop-duration)))
    (list (* 4 (cos (* cur-time-throug-loop scale)))
          (* 6 (sin (* cur-time-throug-loop scale)))
          -20)))

(defun bottom-circle-offset (ellapsed-time)
  (let* ((loop-duration 12)
         (scale (/ (* 2 pi) loop-duration))
         (cur-time-throug-loop (mod ellapsed-time loop-duration)))
    (list (* 5 (cos (* cur-time-throug-loop scale)))
          -3.5
          (- (* 5 (sin (* cur-time-throug-loop scale))) 20))))

(defparameter *offset-funcs* (list #'stationary-offset #'oval-offset #'bottom-circle-offset))

(defun construct-matrix (transform)
  (let ((mat (identity-matrix)))
    (loop for i below 3
       do (setf (mref mat i 3) (coerce (nth i transform) 'float)))
    mat))

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

(defmethod render ((window main-window))
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (use-program *programs-dict* :program)

  (gl:bind-vertex-array *vao*)

  (loop
     with ellapsed-time = (time-since-start)
     for offset-func in *offset-funcs*
     for transform = (funcall offset-func ellapsed-time)
     do
       (uniform :mat :model-to-camera-matrix (vector (construct-matrix transform)))
       (%gl:draw-elements :triangles
                          (/ *index-data-size* (cffi:foreign-type-size :short))
                          :unsigned-short
                          0))
  (gl:bind-vertex-array 0)
  (gl:use-program 0))

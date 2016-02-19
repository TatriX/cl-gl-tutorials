(in-package :gl-tutorial.hello)

;;; This requires your graphics card to support GL 3.3 / GLSL 3.30!

;;; Renders a rotating cube to the screen which can be moved with the mouse by holding
;;; down the left mouse button. Also you may zoom in and out with the mousewheel.


;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
;;; (sdl2.kit:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:cube-window)
;;;
;;; After you close a window, it will be collected at some point.

;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).


(defclass hello-window (gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))


;;Data:--------------------------------------------------------------------------


(defparameter *vertex-positions-contents* '(+0.75 +0.75 0.0 1.0
                                            +0.75 -0.75 0.0 1.0
                                            -0.75 -0.75 0.0 1.0))

(defparameter *vertex-positions*
  (cffi:foreign-alloc
   :float
   :initial-contents
   *vertex-positions-contents*))

;;Shader------------------------------------------------------------------------

;; the returned dictionary with the programs can be used like so:
;; (1) get the program directly (find-program <compiled-dictionary> <program-name>)
;; (2) or just use it directly (use-program <compiled-dictionary> <program-name>)
;;     also (use-program 0) works
(defun load-shaders ()
  (defdict shaders (:shader-path
                    (merge-pathnames
                     #p "01-hello/shaders/" (asdf/system:system-source-directory :gl-tutorials)))
    ;; instead of (:file <path>) you may directly provide the shader as a string containing the
    ;; source code
    (shader hello-v :vertex-shader (:file "hello.vert"))
    (shader hello-f :fragment-shader (:file "hello.frag"))
    ;; here we compose the shaders into programs, in this case just one ":basic-projection"
    (program :hello () ;<- UNIFORMS!
             (:vertex-shader hello-v)
             (:fragment-shader hello-f)))
  ;; function may only run when a gl-context exists, as its documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))

;; to be understood while reading the LOAD-SHADER function
;; example: (uniform :vec :<name-of-uniform> <new-value>)
(defgeneric uniform (type key value)
  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :mat)) key value)
    ;; nice, transpose is NIL by default!
    (uniform-matrix *programs-dict* key 4 value NIL)))

(defvar *position-buffer-object*)

(defun initialize-vertex-buffer ()
  (setf *position-buffer-object* (gl:gen-buffer))
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:buffer-data :array-buffer (* 4 (length *vertex-positions-contents*)) *vertex-positions* :static-draw)
  (gl:bind-buffer :array-buffer 0))

;;utils-------------------------------------------------------------------------

(defun framelimit (window &optional (fps 60))
  "Issues SDL2:DELAY's to get desired FPS."
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
          (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
        (sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))


(defun display-fps (window)
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
        (format t "FPS: ~A~%" (float (/ frames seconds)))
        (setf frames 0)
        (setf start-time (get-internal-real-time))))))

;;init code---------------------------------------------------------------------

(defvar *vao* 0)

(defmethod initialize-instance :after ((w hello-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.

  ;; if you (setf (idle-render window) t) it'll call RENDER as fast as
  ;; possible when not processing other events - suitable for games
  (setf (idle-render w) t)
  (gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer-bit)

  (gl:viewport 0 0 800 600)

  ;; with culling
  ;; (gl:enable :cull-face)
  ;; (gl:cull-face :back)
  ;; (gl:front-face :cw)

  (initialize-program)
  (initialize-vertex-buffer)
  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*))

;;Rendering----------------------------------------------------------------------

(defmethod render ((window hello-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer-bit)

  (use-program *programs-dict* :hello)

  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 4 :float :false 0 0)

  (gl:draw-arrays :triangles 0 (/ (length *vertex-positions-contents*) 4))

  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)

  (display-fps window)
  (framelimit window 60))

;;Events------------------------------------------------------------------------

(defmethod close-window ((window hello-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod keyboard-event ((window hello-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case scancode
      (:scancode-escape (close-window window))
      (:scancode-q (close-window window)))))

(defmethod mousebutton-event ((window hello-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))


(defparameter *window* nil)

(defun main ()
  (sdl2.kit:start)
  (setf *window* (make-instance 'hello-window)))
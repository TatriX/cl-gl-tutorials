(in-package :gl-tutorial.main)

(defclass main-window (gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defvar *programs-dict*)

(defgeneric uniform (type key value)
  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :float)) key value)
    (uniformf *programs-dict* key value))

  (:method ((type (eql :mat)) key value)
    (uniform-matrix *programs-dict* key 4 value nil)))


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


(defmethod close-window ((window main-window))
  (format t "Bye!~%")
  (call-next-method))

(defmethod keyboard-event ((window main-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case scancode
      (:scancode-escape (close-window window))
      (:scancode-q (close-window window)))))

(defmethod mousebutton-event ((window main-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))


(defparameter *window* nil)

(defparameter *start-time* 0)

(defun time-since-start ()
  (/ (- (get-internal-real-time) *start-time*) internal-time-units-per-second))


(defun main ()
  (sdl2.kit:start)
  (setf *start-time* (get-internal-real-time))
  (setf *window* (make-instance 'main-window)))

(defmethod render :after ((window main-window))
  (display-fps window)
  (framelimit window 60))

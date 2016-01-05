;;;; gamejam.lisp

(in-package #:gamejam)


;; controllers
(defvar *controllers*)
(defvar *haptic*) 
(defvar *joystick-dead-zone* 5000)
(defvar *movement-multiplier* 1/500)


(defun find-controllers ()
  (setf *controllers* nil
	*haptic* nil)

  (sdl2-ffi.functions::sdl-game-controller-add-mappings-from-rw (sdl2::sdl-rw-from-file  "gamecontrollerdb.txt" "rw") 1)
  (format t "Opening game controllers.~%")
  (finish-output)
  ;; open any game controllers
  

  (loop for i from 0 upto (- (sdl2:joystick-count) 1)
     do (when (sdl2:game-controller-p i)
	  (format t "Found gamecontroller: ~a~%"
		  (sdl2:game-controller-name-for-index i))
	  
	  (let* ((gc (sdl2:game-controller-open i))
		 (joy (sdl2:game-controller-get-joystick gc)))
	    (setf *controllers* (acons i gc *controllers*))
	    (when (sdl2:joystick-is-haptic-p joy)
	      (let ((h (sdl2:haptic-open-from-joystick joy)))
		(setf *haptic* (acons i h *haptic*))
		(sdl2:rumble-init h))))))
  
  (format t "Controlers found: ~A~%" *controllers*))

;;;; gamejam.lisp

(in-package #:gamejam)

(defmacro with-main (&body body)
  "Enables REPL access via UPDATE-SWANK in the main loop using SDL2. Wrap this around
the sdl2:with-init code."
  ;;TODO: understand this. Without this wrapping the sdl:with-init the sdl thread
  ;; is an "Anonymous thread" (tested using sb-thread:*current-thread*), while applying
  ;; this makes *current-thread* the same as the one one when queried directly from the
  ;; REPL thread: #<SB-THREAD:THREAD "repl-thread" RUNNING {adress...}>
  `(sdl2:make-this-thread-main
    (lambda ()
      ;; does work on linux+sbcl without the following line:
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))


(defun make-window (&optional (width 5) (height 5))
  
  (let ((*local-stdout* *standard-output*)
	(*local-input* *standard-input*))
    
    (with-main
      (sdl2:with-init (:everything)
	(let ((*standard-output* *local-stdout*)
	      (*standard-input* *local-input*))	    
	  
	  (format t "Using SDL Library Version: ~D.~D.~D~%"
		  sdl2-ffi:+sdl-major-version+
		  sdl2-ffi:+sdl-minor-version+
		  sdl2-ffi:+sdl-patchlevel+)
	  (finish-output)
	  
	  (find-controllers)
	  
	  (sdl2:with-window (win :flags '(:shown :opengl :resizable))
	    (sdl2:with-gl-context (gl-context win)
	      
	      ;; basic window/gl setup
	      (format t "Setting up window/gl.~%")
	      (finish-output)
	      (sdl2:gl-make-current win gl-context)
	      (init 10 10)
	      
	      (on-window-resize 800 600)
	      (finish-output)
	      
	      
	      (sdl2:with-event-loop (:method :poll)
		
		(:keyup (:keysym keysym) (on-key-up keysym))
		
		(:controlleraxismotion
		 (:which controller-id :axis axis-id :value value)
		 
		 (cond ((= axis-id 0)
			(setf (first *direction*)
			      (if (> (abs value) *joystick-dead-zone*) (* value *movement-multiplier*) 0)))

		       ((= axis-id 1)
			(setf (second *direction*) (if (> (abs value) *joystick-dead-zone*) (* value *movement-multiplier*) 0)))))		 

		(:controllerbuttondown
		 (:which controller-id)

		 (let ((h (cdr (assoc controller-id *haptic*))))	   
		   (when *jump*
		     (let ((v (ode::body-get-linear-vel *body*)))
		       (cl-ode:body-set-linear-vel *body* (aref v 0) *jump-speed* (aref v 2))
		       (when h
			 (sdl2:rumble-play h 1.0 100))))))

		(:windowevent (:event raw-event :data1 d1 :data2 d2)
			      (let ((event (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id raw-event)))
				
				(when (eql event :RESIZED)
				  (on-window-resize d1 d2))))
		(:idle ()
		       (main-loop)
		       
		       (sdl2:gl-swap-window win)
		       (clinch::update-swank))
		
		(:quit () t))
	      
	      (uninit)
	      
	      (format t "Closing opened game controllers.~%")
	      (finish-output)
	      ;; close any game controllers that were opened
	      ;; as well as any haptics
	      (loop for (i . controller) in *controllers*
		 do (progn
		      (format t "sdl2:haptic-close~%")
		      (sdl2:haptic-close (cdr (assoc i *haptic*)))
		      
		      (format t "sdl2:game-controller-close~%")
		      (sdl2:game-controller-close controller))))))))))

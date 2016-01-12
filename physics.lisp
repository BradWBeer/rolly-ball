;;;; gamejam.lisp

(in-package #:gamejam)

;; physics stuff...
(defvar *world*)
(defvar *space*)
(defvar *body*)
(defvar *plane*)
(defvar *sphere*) 


(defun physics-init () 
  ;; Initialize
  (ode:init)	   

  ;; Create a world to play in...and set defaults...
  (setf *world* (ode::world-create))
  (ode::world-set-defaults *world*)
  (ode:world-set-gravity *world* 0 -10 0)

  ;; Create a space to sort objects intelligently. 
  (setf *space* (ode::quad-tree-space-create nil (ode::make-vector3 0.0 0.0 0.0 0.0) (ode::make-vector3 50.0 50.0 50.0 0.0) 10))
  ;;(setf *space* (ode::hash-space-create nil))

  ;; create a physics body (has mass and such)
  (setf *body* (ode::body-create *world*))

  ;; Create an immovable plane (half the entire space
  (setf *plane* (ode::create-plane *space* 0 1 0 0))
  (setf (ode::surface-bounce *plane*) 1)
  (setf (ode::surface-bounce-vel *plane*) .1)

  ;; Create a sphere...with properties
  (setf *sphere* (ode::sphere-create *space* .5))
  (setf (ode::surface-mu *sphere*) .1)
  (setf (ode::surface-bounce *sphere*) .5)
  (setf (ode::surface-bounce-vel *sphere*) .1)
  (ode:body-set-damping *body* .01)

  ;; Set the body's mass to a sphere shape...
  (let ((m (make-instance 'ode::mass)))
    (ode:mass-set-sphere m .5 1)
    (ode:body-set-mass *body* m))

  ;; Add the sphere collision shape to the body.
  (ode:geom-set-body *sphere* *body*)
  
  ;; Set the body's position
  (ode:body-set-position *body* 0 5 0)

  ;; Add a move handler to watch it go...
  (setf (ode::move-handler *body*)
	(lambda (x)
	  (declare (ignore x))
	  (setf (clinch:transform sphere-node) (sb-cga:Matrix*  (cl-ode::body-get-transform *body*) (sb-cga:scale (sb-cga:vec .5 .5 .5))))))

  ;; Add a collision handler... 
  (setf (ode::collision-handler *sphere*)
  	(lambda (this that contact)
	  ;;(declare (ignore contact))

	  ;; when the sphere collides with the ground, then allow it to jump. 
	  ;; The ground should have a normal vector close to (0 1 0). 
	  ;; Later I might dot product the jump vector with this.	  
	  (when (or (and (eq this *sphere*) (eq that *plane*))
		    (and (eq this *plane*) (eq that *sphere*)))
	    
	    (let* ((geom (cffi:foreign-slot-pointer contact '(:struct ode::contact-struct) 'ode::geom))
		   (normal (subseq (cffi:foreign-slot-value geom '(:struct ode::Contact-Geometry-struct) 'ode::normal) 0 3)))
	      
	      (when (> (aref normal 1) .75)
		(setf *jump* t))))))
)

(defun make-physics-cylinder (pos &optional (height 2))
  (let ((pc (ode::cylinder-create  *space* 1 4)))
    (ode::geom-set-rotation pc (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0) (coerce (/ pi 2) 'single-float)))
    (setf (ode::ghost pc) t)
    (ode:geom-set-position pc (first pos) height (second pos))
    pc))



(defun reset-physics (&optional x y z)
  (ode::body-enable *body*)
  (ode::body-set-position *body*
			  (or x 0)
			  (or y 5)
			  (or z 0))
  (ode:body-set-linear-vel *body* 0 0 0) 
  (ode:body-set-rotation *body* (make-array 12 :element-type 'single-float :initial-contents '(1.0 0.0 0.0 0.0
											       0.0 1.0 0.0 0.0
											       0.0 0.0 0.0 1.0))))
(defun add-force (body x y z)
  (unless (cl-ode:body-is-enabled body) (cl-ode::body-enable body))
  (cl-ode::body-add-force body x y z))

(defun uninit-physics ()
  
  (ode::world-destroy *world*)
  (ode::space-destroy *space*)
  
  (setf *world*          nil
	*space*          nil
	*body*           nil
	*sphere*         nil
	*plane*          nil)
  
  (ode::uninit))

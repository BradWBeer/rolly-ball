;;;; gamejam.lisp

(in-package #:gamejam)

(defvar geo-node nil)
(defvar overlay-node nil)
(defvar sphere-node nil)
(defvar cylinder-node-1 nil)
(defvar cylinder-node-2 nil)
(defvar cylinder-node-3 nil)
(defvar cylinder-node-4 nil)
(defvar overlay-node-1 nil)
(defvar overlay-node-2 nil)
(defvar overlay-node-3 nil)
(defvar overlay-node-4 nil)
(defvar target-node nil)
(defvar ground-node nil)

(defvar *lambda-texture* nil)

(defvar *lose-screen* nil)
(defvar *lose-screen-node* nil)
(defvar *lose-texture* nil)

(defvar overlay-indexes nil)
(defvar overlay-vertices nil)
(defvar overlay-texcoords nil)

(defvar geo-fbo nil)
(defvar depth-buffer nil)

(defvar sphere nil)
(defvar cube nil)
(defvar cylinder nil)
(defvar overlay-entity-1 nil)
(defvar overlay-entity-2 nil)
(defvar overlay-entity-3 nil)
(defvar overlay-entity-4 nil)
(defvar target nil)
(defvar ground nil)

(defvar *maze* nil)
(defvar *static-objects* nil)

(defvar *manual-objects* nil)
(defvar *queue* nil)
(defvar *laser1-position* nil)

(defvar *direction* '(0 0))
(defvar *keyboard-direction* '(0 0))
(defvar *jump* nil)
(defvar *jump-speed* 4)
(defvar *game-over* nil)

(defun make-local-path (file)
  (concatenate 'string 
	       (directory-namestring
		(asdf:system-relative-pathname :gamejam "gamejam.asd"))
	       file))

(defun eval-from-file (file)
  (eval
   (read-from-string
    (alexandria:read-file-into-string
     (make-local-path file)))))


(defun position-cylinder (node x y pc &optional (height 2))
  (clinch:set-identity-transform node)
  (clinch:rotate node (clinch:degrees->radians 90) 1 0 0)
  (clinch:scale node 1 height 1)
  (clinch:translate node x height y)
  (ode:geom-set-position pc x height y) 
  t)
  

(defun lerp (x y distance)
  (let ((1-distance (- 1 distance)))
    (if (and (numberp x)
	     (numberp y))
	(+ (* x 1-distance) (* y distance))
	(map 'list (lambda (a b)
		     (+ (* a 1-distance) (* b distance)))
	     x y))))

(defun make-move-cylinder-func (node pc start end speed handler)
  (if (< (sb-cga:vec-length (sb-cga:vec- start end))
	 speed)
      (lambda ()
	(and handler (funcall handler) nil))
      (let* ((pos      start)
	     (movement (sb-cga:vec* 
			(sb-cga:normalize 
			 (sb-cga:vec- end start))
			speed)))
	(lambda ()
	  (let ((distance (sb-cga:vec-length (sb-cga:vec- pos end))))
	    
	    (if (<  distance speed)
		(progn (and handler (funcall handler))
		       (position-cylinder node (aref end 0) (aref end 2) pc)
		       nil)
		(progn
		  (setf pos 
			(sb-cga:vec+ pos movement))
		  (position-cylinder node (aref pos 0) (aref pos 2) pc))))))))


   
(defun find-valid-movement (y x)
  (destructuring-bind (w h) (array-dimensions *maze*)
    (let ((xx (1+ x))
	  (yy (1+ y)))
      (remove-if #'null
		 (list (when (and (> yy 0) (aref *maze* xx (1- yy))) :left)
		       (when (and (> xx 0) (aref *maze* (1- xx) yy)) :up)
		       (when (and (< yy (1- h)) (aref *maze* xx (1+ yy))) :right)
		       (when (and (< xx (1- w)) (aref *maze* (1+ xx) yy)) :down))))))

(defun get-random-valid-direction (x y &optional last)
  (let* ((lst (find-valid-movement x y))
	 (r (alexandria:random-elt lst))
	 (ret (cond
		((eql r :left) '(-1 0))
		((eql r :up) '(0 -1))
		((eql r :right) '(1 0))
		((eql r :down) '(0 1)))))

    (if (and last
	     (equal last (map 'list #'- ret))
	     (> (length lst) 1))
	(get-random-valid-direction x y last)
	ret)))
    

(defun add-laser-movement (node geom start &optional last-dir)
  (push 
   (let* ((r (get-random-valid-direction (first start) (second start) last-dir))
	  (end (map 'list #'+ start r)))
     ;;(format t "Start: ~A End: ~A R:~A~%" start end r)
     (make-move-cylinder-func 
      node
      geom
      (maze->vector (first start) (second start))
      (maze->vector (first end) (second end))
      .1
      (lambda ()
	(add-laser-movement node geom end r)
	nil)))
   *queue*))

    

(defun make-game-maze (width height)
  
  (setf *maze* (make-maze width height))
  (let ((w/2 (ash width -1))
	(h/2 (ash height -1)))

    (loop for i below (array-dimension *maze* 0)
       do (fresh-line)
	 (loop for j below (array-dimension *maze* 1)
	    unless (aref *maze* i j)
	    do (make-cube (* (- j h/2) 2)
			  (* (- i w/2) 2))))
    *maze*))

(defun maze->position (x y)
  (let ((dims (array-dimensions *maze*)))
    (values (- 2 (* 2 (- (ash (first dims) -2) x)))
	    (- 2 (* 2 (- (ash (second dims) -2) y))))))
  
(defun maze->vector (x y &optional (up 2))
  (multiple-value-bind (a b) (maze->position x y)
    (sb-cga:vec (float a) (float up) (float b))))

(defun reset-maze (w h)
  (destroy-all-statics)
  (setf *random-state* (make-random-state))
  (let ((maze (make-game-maze w h)))
    (multiple-value-bind (x y) (maze->position 0 0)
      (ode:body-set-position *body* x .5 y)
    maze)))

;;
(defun add-cylinder (pos)
  (let ((c (make-instance 'clinch:node))
	(pc (make-physics-cylinder pos)))
    (clinch:add-child c cylinder)
    (clinch:add-child geo-node c)
    (add-laser-movement c pc pos)
    (setf (ode::collision-handler pc)
	  (lambda (this that contact)
	   (print "Death")
	   (setf *game-over* t)))
    ))


  ;; init game objects...
(defun init (width height)

  (init-sound)
  (init-opengl)
  
  ;; Main geometry node
  (setf geo-node (make-instance 'clinch:node))
  (clinch:translate geo-node 0 -2 -10)

  ;; create the sphere
  (setf sphere (eval-from-file "assets/mesh/sphere.lisp"))

  (setf (clinch:shader sphere) (lambda () tex-light-shader))
  (setf sphere-node (make-instance 'clinch:node))
  (clinch:add-child sphere-node sphere)
  (clinch:add-child geo-node sphere-node)

  ;; create the cube
  (setf cube (eval-from-file "assets/mesh/cube.lisp"))
  (setf (clinch:shader cube) (lambda () tex-light-shader))

  ;; create the cylinder
  (setf cylinder (eval-from-file "assets/mesh/cylinder.lisp"))
  (setf (clinch:shader cylinder) (lambda () tex-light-shader))

  (let ((tex (make-texture 20 20)))
    (setf (clinch:render-value cylinder "t1") tex)
    (draw-overlay tex '(1 0 0 1)))


  ;; create the ground
  (setf ground (eval-from-file "assets/mesh/ground.lisp"))
  (setf (clinch:shader ground) (lambda () tex-light-shader))
  (setf ground-node (make-instance 'clinch:node))
  (clinch:add-child ground-node ground)
  (clinch:add-child geo-node ground-node)

  (clinch:set-identity-transform ground-node)
  (clinch:scale ground-node 100 100 0)
  (clinch:rotate ground-node (/ 3.141592653 -2) 1 0 0)

  (setf overlay-indexes (make-instance 'clinch:buffer :qtype :unsigned-int :target :element-array-buffer :Stride 1 
						       :data '(0 1 2 0 2 3)))
  (setf overlay-vertices (make-instance 'clinch:buffer :Stride 3
							:data (map 'list (lambda (x) (coerce x 'single-float)) '( -1   1 0
														 -1  -1 0
														 1  -1 0
														 1   1 0))))
  (setf overlay-texcoords (make-instance 'clinch:buffer :stride 2
					 :data '(0.0 0.0
						 0.0 1.0
						 1.0 1.0
						 1.0 0.0)))

  ;; make overlay node
  (setf overlay-node (make-instance 'clinch:node))
  (multiple-value-setq (overlay-node-1 overlay-entity-1) (make-overlay overlay-node (lambda () tex-shader) 800 800))
  (multiple-value-setq (overlay-node-2 overlay-entity-2) (make-overlay overlay-node (lambda () tex-shader) 800 800))
  (multiple-value-setq (overlay-node-3 overlay-entity-3) (make-overlay overlay-node (lambda () tex-shader) 800 800))
  (multiple-value-setq (overlay-node-4 overlay-entity-4) (make-overlay overlay-node (lambda () tex-shader) 800 800))

  (multiple-value-setq (*lose-screen-node* *lose-screen*) (make-overlay overlay-node (lambda () tex-shader) 800 800))
  (setf tex
	(setf (clinch:render-value *lose-screen* "t1") *lose-texture*))
  

  (multiple-value-setq (target-node target)
    (make-overlay nil
		  (lambda ()
		    deferred-directional-light-shader)
		  800 800))					   


  (setf geo-fbo (make-instance 'clinch:frame-buffer))
  
  (physics-init)

  (reset-maze width height)


  (let* ((pos (map 'list (lambda (x)
			     (- x 3))
		      (array-dimensions *maze*))))
	 
    (setf cylinder-node-1 (add-cylinder pos))
    (setf cylinder-node-2 (add-cylinder pos))
    (setf cylinder-node-3 (add-cylinder pos))
    (setf cylinder-node-4 (add-cylinder pos)))

  (setf *channel* (sdl2-mixer:play-music *music*)))

(defun entity-replace-texture (entity id width height color)
  (let ((tex (clinch:render-value entity id)))
    (when tex (clinch:unload tex))
    (setf tex
	  (setf (clinch:render-value entity id)
		(make-texture width height)))
    (draw-overlay tex color)))

(defun place-node (node size location)
  (clinch:set-identity-transform node)
  (apply #'clinch:scale node size)
  (apply #'clinch:translate node location))



(defun on-window-resize (width height)
  (setf win-size (list width height))

  (clinch::quick-set viewport 0 0 width height)
  (setf projection-matrix (clinch::make-perspective-transform (/ clinch::+pi+ 4) (/ width height) .1 1000))
  (clinch::render viewport :projection projection-matrix)

  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (setf ortho-matrix (clinch:make-orthogonal-transform width height .25 1000))
  
  (entity-replace-texture overlay-entity-1 "t1" width height '(1 0 0 1))
  (place-node overlay-node-1
	      (list (/ width 5) (/ height 5) 0)
	      (list (/ width -4)  (/ height 4) 5))

  (entity-replace-texture overlay-entity-2 "t1" width height '(0 1 0 1))
  (place-node overlay-node-2
	      (list (/ width 5) (/ height 5) 0)
	      (list (/ width 4)  (/ height 4) 5))

  (entity-replace-texture overlay-entity-3 "t1" width height '(0 0 1 1))
  (place-node overlay-node-3
	      (list (/ width 5) (/ height 5) 0)
	      (list (/ width -4)  (/ height -4) 5))

  (entity-replace-texture overlay-entity-4 "t1" width height '(1 1 0 1))
  (place-node overlay-node-4
	      (list (/ width 5) (/ height 5) 0)
	      (list (/ width 4)  (/ height -4) 5))

  (entity-replace-texture ground "t1" width height '(1 1 1 1))
  
  (place-node target-node (list (/ width 2) (/ height 2) 0)
	      (list 0 0 0))

  (place-node *lose-screen-node* (list (/ width 2) (/ height 2) 0) '(0 0 0))

  (clinch:bind geo-fbo)

  (when depth-buffer (clinch:unload depth-buffer))
  (setf depth-buffer
  	(make-instance 'clinch:texture 
  		       :width (first win-size)
  		       :height (second win-size)
  		       :internal-format :depth-component32
  		       :format :depth-component
  		       :qtype :unsigned-int 
  		       :stride 1
		       :depth-texture-mode :intensity
		       :texture-compare-mode :compare-r-to-texture
		       :texture-compare-function :lequal))

  (gl:bind-texture :texture-2d (clinch:tex-id depth-buffer))
  (gl:framebuffer-texture-2d :DRAW-FRAMEBUFFER :depth-attachment :texture-2d (clinch:tex-id depth-buffer) 0)

  (clinch::add-color-buffer geo-fbo (clinch:render-value overlay-entity-1 "t1") 0)
  (clinch::add-color-buffer geo-fbo (clinch:render-value overlay-entity-2 "t1") 1)
  (clinch::add-color-buffer geo-fbo (clinch:render-value overlay-entity-3 "t1") 2)
  (clinch::add-color-buffer geo-fbo (clinch:render-value overlay-entity-4 "t1") 3)
  
  (clinch:bind geo-fbo)
  
  (gl:draw-buffers '(:color-attachment0 :color-attachment1 :color-attachment2 :color-attachment3))
  
  (gl:enable :blend :depth-test :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (clinch:unbind geo-fbo)

  (setf (clinch:render-value target "raw")
	(clinch:render-value overlay-entity-1 "t1"))
  (setf (clinch:render-value target "normals")
	(clinch:render-value overlay-entity-3 "t1")))


(defun on-key-down (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
	(sym (sdl2:sym-value keysym))
	(mod-value (sdl2:mod-value keysym)))

    (cond
      ((sdl2:scancode= scancode :scancode-left) (setf (first *keyboard-direction*) -65))
      ((sdl2:scancode= scancode :scancode-right) (setf (first *keyboard-direction*) 65))
      ((sdl2:scancode= scancode :scancode-up)   (setf (second *keyboard-direction*) -65))
      ((sdl2:scancode= scancode :scancode-down) (setf (second *keyboard-direction*) 65))
      ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))
      ((sdl2:scancode= scancode :scancode-m)
       (setf *play-music* (not *play-music*))
       (if *play-music*
	   (sdl2-mixer:play-music *music*)
	   (sdl2-mixer:halt-music)))
      ((sdl2:scancode= scancode :scancode-space)   
       
       (if *game-over*
	   (progn
	     (game::reset-maze 10 10)
	     (setf *game-over* nil))
	   (when *jump*
	     (let ((v (ode::body-get-linear-vel *body*)))
	       (cl-ode:body-set-linear-vel *body* (aref v 0) *jump-speed* (aref v 2)))))))))


       

(defun on-key-up (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
	(sym (sdl2:sym-value keysym))
	(mod-value (sdl2:mod-value keysym)))

    (cond
      ((sdl2:scancode= scancode :scancode-left) (setf (first *keyboard-direction*) 0))
      ((sdl2:scancode= scancode :scancode-right) (setf (first *keyboard-direction*) 0))
      ((sdl2:scancode= scancode :scancode-up) (setf (second *keyboard-direction*) 0))
      ((sdl2:scancode= scancode :scancode-down) (setf (second *keyboard-direction*) 0))
      ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))
	   
      )))

(defun main-loop ()

  ;; move the manual objects 
  (setf *manual-objects* 
	(loop for i in *manual-objects*
	   if (funcall i)
	   collect i))

  (setf *manual-objects* (append *manual-objects* *queue*))
  (setf *queue* nil)
  
  ;; do physics
  (unless *game-over* 
    (setf *jump* nil)
    (let ((dir (map 'list (lambda (a b)
			    (if (>= (abs a) (abs b)) a b))
		    *keyboard-direction* *direction*)))
      (add-force *body* (first dir) 0 (second dir)))
    (ode:physics-step *world* *space*))

  ;; set the camera
  (let ((pos (ode:body-get-position *body*)))
    (set-camera (- (aref pos 0)) -15 (- -4 (aref pos 2)) 60 1 0 0))

  ;; do the render pipeline...
  (opengl-clear-main-window)
  (render-g-buffers)
  (render-to-target-texture)

  ;; show g-buffer overlays when asked
  (when *game-over*
    (render-overlays)))


(defun uninit()
  (destroy-all-statics)
  (uninit-physics)
  (unload-opengl))

(defun start (&optional (width 20) (height 20))
  (make-window width height))

(defun rot (x &optional (y 0) (z -2))
	   (clinch:set-identity-transform sphere-node)
   
	   (clinch:rotate sphere-node (clinch:degrees->radians x)
			  0 1 0)
	   (clinch:rotate sphere-node (clinch:degrees->radians y)
			  1 0 0)
	   (clinch:translate sphere-node 0 0 z))
	    

(defun set-camera (x y z angle xx yy zz)
  (clinch:set-identity-transform geo-node)
  (clinch:translate geo-node x y z)
  (clinch:rotate geo-node (clinch:degrees->radians angle) xx yy zz))

    


;;;; gamejam.lisp

(in-package #:gamejam)

(defvar tex-shader nil)
(defvar tex-light-shader nil)
(defvar deferred-directional-light-shader nil)
(defvar viewport nil)
(defvar ortho-matrix nil)
(defvar projection-matrix nil)

(defmacro update-shader (var path)
  (let ((tmp-var (gensym)))
    `(let ((,tmp-var (eval-from-file ,path)))
       (when ,tmp-var
	 (when ,var
	   (clinch:unload ,var)
	   (setf ,var nil))
	 (setf ,var ,tmp-var)))))

(defun draw-overlay (texture color)
  (clinch::with-context-for-texture  (texture :width-var w :height-var h)
    (apply #'clinch:clear-cairo-context color)))

(defun make-texture (width height)
  (make-instance 'clinch:texture 
		 :width width
		 :height height
		 :stride 4
		 :qtype :unsigned-char
		 :target :pixel-unpack-buffer))

(defun make-overlay (root shader width height)

  (let* ((entity (make-instance 'clinch:entity
			       :shader shader
			       :indexes overlay-indexes
			       :values   (copy-tree 
					  `((:attribute "tc1" ,overlay-texcoords)
					    (:attribute "v" ,overlay-vertices)
					    (:uniform "P" :projection)
					    (:uniform "M" :Model)
					    (:uniform "raw" nil)
					    (:uniform "lightDirection" (0.5772705 0.5772705 0.5772705))
					    (:uniform "lightIntensity" (0.5 0 0))
					    (:uniform "lightAmbient"   (0 0 0))
					    (:uniform "normals" nil)
					    (:uniform "t1" nil)))))
	 (node (make-instance 'clinch:node)))
    (clinch:add-child node entity)
    (when root (clinch:add-child root node))
    (values node entity)))

(defun init-opengl ()
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  
  (setf *lambda-texture* (clinch::create-texture-from-png (make-local-path "/assets/img/lambda.png")))
  (setf *lose-texture* (clinch::create-texture-from-png (make-local-path "/assets/img/lose.png")))
  
  (update-shader tex-shader "assets/shaders/simple-texture-shader.lisp")
  (update-shader tex-light-shader "assets/shaders/deferred-g-buffer-shader.lisp")
  (update-shader deferred-directional-light-shader "assets/shaders/deferred-directional-light-shader.lisp")

  (setf viewport (make-instance 'clinch:viewport)))


(defun resize-opengl (width height)

  (clinch::quick-set viewport 0 0 width height)
  (setf projection-matrix (clinch::make-perspective-transform (/ clinch::+pi+ 4) (/ width height) .1 1000))
  (clinch::render viewport :projection projection-matrix)
  
  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (setf ortho-matrix (clinch:make-orthogonal-transform width height .25 1000)))

(defun opengl-clear-main-window ()
  ;; Clear the main window
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-stencil 0)
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))


(defun render-g-buffers ()
    ;; Start making the geometry buffers
  (clinch:bind geo-fbo)

  ;; Enable depth-test and allow writing to the depth buffer for geometry pass
  (gl:depth-mask :true)
  (gl:enable :depth-test)

  ;; Clear the color, depth and stencil buffers
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)

  ;; no blending for geometry pass
  (gl:disable :blend)

  ;; render the g-buffers for the main scene
  (clinch:render geo-node :projection projection-matrix)

  ;; Leave the gpass and return to window rendering...
  (clinch:unbind geo-fbo))

(defun render-to-target-texture ()
  
  ;; Disable depth test and inhibit modifying the depth buffer
  (gl:disable :depth-test)
  (gl:depth-mask :false)
  
  (gl:enable :blend)
  (gl:blend-equation :func-add)
  (gl:blend-func :one :one)

  (setf (clinch:render-value target "lightIntensity")
	'(0.0 .5 0.0)
	(clinch:render-value target "lightDirection")
	'(0.5772705 0.5772705 -0.5772705)
	(clinch:render-value target "lightAmbient")
		'(.35 .35 .35))

  (clinch:render target-node :projection ortho-matrix)

  (setf (clinch:render-value target "lightIntensity")
	'(.0 .0 .5)
	(clinch:render-value target "lightDirection")
	'(-0.5772705 -0.5772705 0.5772705)
		(clinch:render-value target "lightAmbient")
		'(0 0 0))
  
  (clinch:render target-node :projection ortho-matrix))

(defun render-overlays ()

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (when *game-over* 
    (clinch:render overlay-node :projection ortho-matrix)))


(defun unload-opengl ()
  (clinch:unload tex-shader)
  (clinch:unload tex-light-shader)
  (clinch:unload deferred-directional-light-shader)
  (clinch:unload *lambda-texture*)
  
  (clinch:unload geo-fbo)
  (clinch:unload depth-buffer)
  
  (clinch:unload overlay-indexes)
  (clinch:unload overlay-vertices)
  (clinch:unload overlay-texcoords)
  
  (clinch:unload sphere)
  (clinch:unload cube)
  (clinch:unload overlay-entity-1)
  (clinch:unload overlay-entity-2)
  (clinch:unload overlay-entity-3)
  (clinch:unload overlay-entity-4)
  (clinch:unload target)
  (clinch:unload ground))



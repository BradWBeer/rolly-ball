;;;; gamejam.lisp

(in-package #:gamejam)

(defclass static-game-object ()
  ((parent-node :initform nil
		:initarg :parent
		:accessor parent)
   (node        :initform (make-instance 'clinch:node)
		:initarg  :node
		:accessor node)
   (geometry    :initform :geometry
		:initarg  :geometry
		:accessor geometry)
   (entity      :initform nil
		:initarg :entity
		:accessor entity)))

(defmethod initialize-instance :after ((this static-game-object) &key x y z)
  " move both the visual and physics object to the same place"

  (with-accessors ((parent parent) (node node) (geometry geometry) (entity entity)) this

    (when parent (clinch:add-child parent node))
    (clinch:translate node x y z)
    (clinch:add-child node entity)
    (ode:geom-set-position geometry x y z)

    (setf (gethash this *static-objects*) this)))


(defmethod destroy ((this static-game-object))
  (with-accessors ((parent parent) (node node) (geometry geometry) (entity entity)) this
    (setf entity nil)
    (when parent (clinch:remove-child parent node))
    (when geometry (ode:geom-destroy geometry))

    (setf parent nil)
    (setf node nil)
    (setf geometry nil)

    (setf (gethash this *static-objects*) nil)))


(defun destroy-all-statics ()
  
  (when *static-objects*
    (maphash (lambda (key value)
     	       (when value (destroy value)))	       
	     *static-objects*))
  (setf *static-objects* (make-hash-table)))

(defun make-cube (x y)

  (let ((box (ode:create-box *space* 2 2 2)))

    (setf (ode::surface-mu box) 1)
    ;;(setf (ode::surface-bounce box) .5)
    ;;(setf (ode::surface-bounce-vel box) .1)
    
    (make-instance 'static-game-object
		   :parent geo-node
		   :geometry box
		   :entity cube
		   :x x :y 1 :z y)))

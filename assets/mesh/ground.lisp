(make-instance 'clinch:entity
	       :indexes (make-instance 'clinch:buffer
				       :qtype :unsigned-int
					     :target :element-array-buffer
					     :Stride 1
					     :data '(0 1 2 0 2 3))
		     :values   `((:attribute "v"  ,(make-instance 'clinch:buffer
								  :Stride 3
								  :data '( -10.0  10.0 0.0
									  -10.0  -10.0 0.0
									  10.0  -10.0 0.0
									  10.0   10.0 0.0)))
				 (:attribute "n" ,(make-instance 'clinch:buffer :Stride 3
								 :data '(0.0 0.0 10.0
									 0.0 0.0 10.0
									 0.0 0.0 10.0
									 0.0 0.0 10.0)))
				 (:uniform "M" :model)
				 (:uniform "P" :projection)
				 (:uniform "ambientLight"   (.2 .2 .2))
				 (:uniform "lightIntensity" (.8 .8 .8))
				 (:uniform "lightDirection" (0.5772705 0.5772705 0.5772705))
				 (:uniform "t1" nil)
				 (:attribute "tc"  ,(make-instance 'clinch:buffer 
								   :Stride 2
								   :data '(0.0 0.0
									   0.0 1.0
									   1.0 1.0
									   1.0 0.0)))))

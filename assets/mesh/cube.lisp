(setf cube 
      (make-instance 'clinch:entity
		     :indexes (make-instance 'clinch:buffer
					     :qtype :unsigned-int
					     :target :element-array-buffer
					     :Stride 1
					     :data '(0  1  2 
						     2  1  3
						     4  5  6
						     6  5  7
						     8  9 10 
						     10  9 11
						     12 13 14 
						     14 13 15
						     16 17 18 
						     18 17 19
						     20 21 22 
						     22 21 23))
		     :values   `((:attribute "v"  ,(make-instance 'clinch:buffer
								  :Stride 3
								  :data '(-1.0 -1.0  1.0
									  1.0 -1.0  1.0
									  -1.0  1.0  1.0
									  1.0  1.0  1.0
									  -1.0 -1.0 -1.0
									  1.0 -1.0 -1.0
									  -1.0 -1.0  1.0
									  1.0 -1.0  1.0
									  -1.0  1.0 -1.0
									  1.0  1.0 -1.0
									  -1.0 -1.0 -1.0
									  1.0 -1.0 -1.0
									  -1.0  1.0  1.0
									  1.0  1.0  1.0
									  -1.0  1.0 -1.0
									  1.0  1.0 -1.0
									  1.0 -1.0  1.0
									  1.0 -1.0 -1.0
									  1.0  1.0  1.0
									  1.0  1.0 -1.0
									  -1.0 -1.0 -1.0 
									  -1.0 -1.0  1.0 
									  -1.0  1.0 -1.0 
									  -1.0  1.0  1.0)))
				 (:attribute "n" ,(make-instance 'clinch:buffer :Stride 3
								 :data '(0.0 0.0 1.0
									 0.0 0.0 1.0
									 0.0 0.0 1.0
									 0.0 0.0 1.0
									 0.0 -1.0 0.0
									 0.0 -1.0 0.0
									 0.0 -1.0 0.0
									 0.0 -1.0 0.0
									 0.0 0.0 -1.0
									 0.0 0.0 -1.0
									 0.0 0.0 -1.0
									 0.0 0.0 -1.0
									 0.0 1.0 0.0
									 0.0 1.0 0.0
									 0.0 1.0 0.0
									 0.0 1.0 0.0
									 1.0 0.0 0.0
									 1.0 0.0 0.0
									 1.0 0.0 0.0
									 1.0 0.0 0.0
									 -1.0 0.0 0.0
									 -1.0 0.0 0.0
									 -1.0 0.0 0.0
									 -1.0 0.0 0.0)))
				 
				 (:uniform "M" :model)
				 (:uniform "P" :projection)
				 (:uniform "ambientLight"   (.2 .2 .2))
				 (:uniform "lightIntensity" (.8 .8 .8))
				 (:uniform "lightDirection" (0.5772705 0.5772705 0.5772705))
				 (:uniform "t1" ,(lambda ()
							 gamejam::*lambda-texture*))
				 (:attribute "tc"  ,(make-instance 'clinch:buffer 
								   :Stride 2
								   :data '(0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0
									   0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0
									   0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0
									   0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0
									   0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0
									   0.0 1.0
									   1.0 1.0
									   0.0 0.0
									   1.0 0.0)))))))))))


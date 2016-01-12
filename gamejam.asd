;;;; gamejam.asd

(asdf:defsystem #:gamejam
  :description "Game for the Lisp GameJam"
  :author "warweasle (Brad Beer)"
  :license "Mega-ultra-free-license. Just don't sue me."
  :depends-on (#:sdl2
	       #:sdl2-mixer
               #:clinch
               #:clinch-slime
               #:clinch-cairo
               #:cl-ode)
  :serial t
  :components ((:file "package")
	       (:file "maze")
	       (:file "window")
	       (:file "physics")
	       (:file "controller")
	       (:file "opengl")
	       (:file "statics")
	       (:file "sound")
               (:file "gamejam")))


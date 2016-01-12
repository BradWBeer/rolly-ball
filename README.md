* Lisp Maze Game in Clinch and CL-ODE

To build, get a lisp with quicklisp, emacs and install slime.

Under quicklisp/local-projects/ "git" the following repos...

git clone https://github.com/BradWBeer/clinch.git
git clone https://github.com/BradWBeer/cl-ode.git
git clone https://github.com/BradWBeer/cl-ode.git
git clone https://github.com/lispgames/cl-sdl2-mixer.git

You may need to build and install the ODE phyics library. The instructions for that are at https://github.com/BradWBeer/cl-ode. Use the build_ode.sh script to build it on Linux. 

Start slime in emacs and type> (ql:quickload :gamejam) (gamejam:start)

Use a controller or arrow keys to move. Use any button or space to jump or restart a game.
How long can you survive without being vaporized by the orbital lasers?

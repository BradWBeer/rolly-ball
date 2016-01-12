;;;; sound.lisp

(in-package #:gamejam)

(defvar *music* nil)
(defvar *channel* nil)
(defvar *play-music* t)

(defun init-sound ()

  (sdl2-mixer:init :mp3)
  (sdl2-mixer:open-audio 22050 :s16sys 2 640)
  (setf *music* (sdl2-mixer:load-music (make-local-path "assets/music/THA_NeuroTransmitter.mp3"))))


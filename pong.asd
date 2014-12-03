;;;; pong.asd

(asdf:defsystem #:pong
  :description "Pong"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL"
  :depends-on (#:lispbuilder-sdl 
	       #:lispbuilder-sdl-ttf
	       #:lispbuilder-sdl-mixer
	       #:cl-fad)
  :serial t
  :components ((:file "package")
               (:file "pong")))


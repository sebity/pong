;;;; pong.asd

(asdf:defsystem #:pong
  :version "1.0"
  :description "A remake of the classic game Pong."
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL"
  :depends-on (#:lispbuilder-sdl 
	       #:lispbuilder-sdl-ttf
	       #:lispbuilder-sdl-mixer
	       #:cl-fad)
  :serial t
  :components ((:file "package")
               (:file "pong")))


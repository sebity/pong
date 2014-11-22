;;;; pong.lisp

(in-package #:pong)

(defparameter *game-width* 600)
(defparameter *game-height* 600)
(defparameter *player-1* nil)
(defparameter *player-2* nil)
(defparameter *ball* nil)
(defparameter *player-1-score* 0)
(defparameter *player-2-score* 0)

;;;; PADDLE class

(defclass paddle ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (v-y :accessor v-y :initarg :v-y :initform 0)
   (spd :accessor spd :initarg :spd)))

(defclass ball ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (v-x :accessor v-x :initarg :v-x :initform 0)
   (v-y :accessor v-y :initarg :v-y :initform 0)
   (spd :accessor spd :initarg :spd)))


(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;; DRAW-BALL function

(defun draw-ball (b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-*
		 (x b) (y b) (w b) (h b))
		:color (sdl:color :r (r b) :g (g b) :b (b b))))


;;;; UPDATE-BALL function

(defun update-ball (b p1 p2)
  (setf (x b) (+ (x b) (* (v-x b) (spd b))))
  (setf (y b) (+ (y b) (* (v-y b) (spd b))))

  (if (> (v-y b) (* (spd b) 2))
      (setf (v-y b) (* (spd b) 2))
      (if (< (v-y b) (- (* (spd b) 2)))
	  (setf (v-y b) (- (* (spd b) 2)))))

  (if (or (< (- (y b) 5) 0) (> (+ (y b) 5) *game-height*))
      (setf (v-y b) (- (v-y b))))

  (if (and (<= (x b) (+ (x p1) (w p1)))
	   (>= (x b) (x p1)))
      (if (and (<= (y b) (+ (y p1) (/ (h p1) 2)))
	       (>= (y b) (- (y p1) (/ (h p1) 2))))
	  (setf (v-x b) (- (v-x b)))))

  (if (and (>= (x b) (- (x p2) (w p2)))
	   (<= (x b) (x p2)))
      (if (and (<= (y b) (+ (y p2) (/ (h p2) 2)))
	       (>= (y b) (- (y p2) (/ (h p2) 2))))
	  (setf (v-x b) (- (v-x b)))))

  (if (<= (x b) 0)
      (update-score 2))
  (if (>= (x b) *game-width*)
      (update-score 1)))



;;;; DRAW-PADDLE function

(defun draw-paddle (p)
  (sdl:draw-box (sdl:rectangle-from-midpoint-*
		 (x p) (y p) (w p) (h p))
		:color (sdl:color :r (r p) :g (g p) :b (b p))))


;;;; MOVE-PLAYER function

(defun move-player (player direction)
  (cond ((eq direction 'up)
	 (if (> (- (y player) (/ (h player) 2)) 0)
	     (setf (y player) (- (y player) (spd player)))))
	((eq direction 'down)
	 (if (< (+ (y player) (/ (h player) 2)) *game-height*) 
	     (setf (y player) (+ (y player) (spd player)))))))


(defun player-ai (c b)
  ;(setf (v-y c) (- (y b) (y c)))

  (if (< (y b) (y c))
      (setf (y c) (- (y c) 2))
      (setf (y c) (+ (y c) 2))))

;;;; PLAYER-1 function

(defun player-1 ()
  (draw-paddle *player-1*))


;;;; PLAYER-2 function

(defun player-2 ()
  (draw-paddle *player-2*))


;;;; BALL function

(defun ball ()
  (update-ball *ball* *player-1* *player-2*)
  (draw-ball *ball*))


;;;; UPDATE-SCORE function

(defun update-score (player)
  (if (= player 1)
      (incf *player-1-score*))
  (if (= player 2)
      (incf *player-2-score*))

  (reset-game))



;;;; DRAW-TEXT function

(defun draw-text (string x y r g b)
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)))


;;;; DISPLAY-SCORE function

(defun display-score ()
  (let ((score (format nil "~a : ~a" *player-1-score* *player-2-score*)))
    (draw-text score (- (/ *game-width* 2) 20) 10 255 255 255)))


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)
  (player-1)
  (player-2)
  (ball)
  (display-score)
  (player-ai *player-2* *ball*)
  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *player-1* (make-instance 'paddle
				  :x 10 :y (/ *game-height* 2)
				  :w 10 :h 40
				  :r 255 :g 0 :b 0
				  :spd 3))
  (setf *player-2* (make-instance 'paddle
				  :x (- *game-width* 10) :y (/ *game-height* 2)
				  :w 10 :h 40
				  :r 0 :g 0 :b 255
				  :spd 3))
  (setf *ball* (make-instance 'ball
			      :x (/ *game-width* 2) :y (/ *game-height* 2)
			      :w 10 :h 10
			      :r 255 :g 255 :b 0
 			      :v-x -1 :v-y 1 :spd 2))) 


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *player-1-score* 0)
  (setf *player-2-score* 0))


;;;; CLEAN-UP function

(defun clean-up ()
  ; Clean up calls go here...
  )


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "Pong")
    (setf (sdl:frame-rate) 30)

    (sdl:enable-key-repeat 50 20)

    (setf sdl:*default-font* (sdl:initialise-font sdl:*font-10x20*))

    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-r (reset-game))
			 (:sdl-key-a (move-player *player-1* 'up))
			 (:sdl-key-z (move-player *player-1* 'down))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (render)))))

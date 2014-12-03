;;;; pong.lisp

(in-package #:pong)

(defparameter *data-root* "src/lisp/pong/")
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))

(defparameter *game-width* 600)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0=entry, 1:ready, 2:in-game, 3:miss, 4:end-level, 5:scores
(defparameter *player-1* nil)
(defparameter *player-2* nil)
(defparameter *ball* nil)
(defparameter *player-1-score* 0)
(defparameter *player-2-score* 0)
(defparameter *players* 1)

(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

(defparameter *terminus-ttf* (make-instance 'SDL:ttf-font-definition
					    :size 24
					    :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

;;;; PADDLE class

(defclass paddle ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (v-y :accessor v-y :initarg :v-y :initform 0.0)
   (spd :accessor spd :initarg :spd)))

(defclass ball ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (v-x :accessor v-x :initarg :v-x :initform 0.0)
   (v-y :accessor v-y :initarg :v-y :initform 0.0)
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

  ; top wall
  (if (or (< (- (y b) 5) 0) (> (+ (y b) 5) *game-height*))
      (progn (setf (v-y b) (- (v-y b)))
	     (play-sound 2)))

  ; player 1 paddle
  (if (and (<= (x b) (+ (x p1) (w p1)))
	   (>= (x b) (x p1)))
      (if (and (<= (y b) (+ (y p1) (/ (h p1) 2)))
	       (>= (y b) (- (y p1) (/ (h p1) 2))))
	  (progn (setf (v-x b) (- (v-x b)))
		 (play-sound 0))))

  ; player 2 paddle
  (if (and (>= (x b) (- (x p2) (w p2)))
	   (<= (x b) (x p2)))
      (if (and (<= (y b) (+ (y p2) (/ (h p2) 2)))
	       (>= (y b) (- (y p2) (/ (h p2) 2))))
	  (progn (setf (v-x b) (- (v-x b)))
		 (play-sound 0))))

  (if (<= (x b) 0)
      (progn (update-score 2)
	     (play-sound 1)))
  (if (>= (x b) *game-width*)
      (progn (update-score 1)
	     (play-sound 1))))


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


;;;; PLAYER-AI function

(defun player-ai (c b)
  (if (< (y b) (y c))
      (setf (y c) (- (y c) (spd c)))
      (setf (y c) (+ (y c) (spd c)))))


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
  (let ((score (format nil "~a   ~a" *player-1-score* *player-2-score*)))
    (draw-text score (- (/ *game-width* 2) 30) 10 255 255 255)))


(defun display-court ()
  (sdl:draw-line-* (ash *game-width* -1) 0 
		   (ash *game-width* -1) *game-height*
		   :color sdl:*white*))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "PONG" (- (/ *game-width* 2) 10) 100 255 255 255)
  (draw-text "Press SPACE to Play" (- (/ *game-width* 2) 100) 300 255 255 0))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (if (zerop *game-state*)
      (progn (setf *player-1-score* 0)
	     (setf *player-2-score* 0)
	     (reset-game)
	     (setf *game-state* 2))))


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)
  (cond ((zerop *game-state*) (display-menu))
	(t (player-1)
	   (player-2)
	   (display-court)
	   (display-score)
	   (ball)
	   (if (= *players* 1)
	       (player-ai *player-2* *ball*))))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *player-1* (make-instance 'paddle
				  :x 10.0 :y (/ *game-height* 2.0)
				  :w 10 :h 40
				  :r 255 :g 0 :b 0
				  :spd 4.5))
  (setf *player-2* (make-instance 'paddle
				  :x (- *game-width* 10.0) :y (/ *game-height* 2.0)
				  :w 10 :h 40
				  :r 0 :g 0 :b 255
				  :spd 4.5))
  (setf *ball* (make-instance 'ball
			      :x (/ *game-width* 2.0) :y (/ *game-height* 2.0)
			      :w 10 :h 10
			      :r 255 :g 255 :b 0
 			      :v-x -1.0 :v-y 0.8 :spd 4.0))) 


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *players* 1)
  (setf *player-1-score* 0)
  (setf *player-2-score* 0)
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 3))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "beep.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "peep.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "plop.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "Pong")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (sdl:enable-key-repeat 20 20)

    ;(setf sdl:*default-font* (sdl:initialise-font sdl:*font-10x20*))
    (unless (sdl:initialise-default-font *terminus-ttf*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-r (reset-game))
			 (:sdl-key-q (setf *game-state* 0))
			 (:sdl-key-1 (setf *players* 1))
			 (:sdl-key-2 (setf *players* 2))
			 (:sdl-key-space (change-game-state))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-a) (move-player *player-1* 'up))
	     (when (sdl:get-key-state :sdl-key-z) (move-player *player-1* 'down))
	     (when (sdl:get-key-state :sdl-key-up) (move-player *player-2* 'up))
	     (when (sdl:get-key-state :sdl-key-down) (move-player *player-2* 'down))
	     (render)))))

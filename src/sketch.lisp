(ql:quickload 'game-sketch)

(defmacro at-screen-center ()
  `(list (ash 960 -1) (ash 540 -1)))

(defmacro from-center (position)
  `(let ((center (at-screen-center)))
     (list (+ (car ,position) (car center))
	   (+ (cadr ,position) (cadr center)))))

;;; Some general values
(defparameter *boundaries* (list (cons 0 960)
				 (cons 0 460)))
(defparameter *lod* :low)
(defparameter *paused* nil)
(defparameter *score* 0)
(defparameter *high* 0)

(defmacro max-x ()
  `(cdr (car *boundaries*)))

(defmacro max-y ()
  `(cdr (cadr *boundaries*)))

;;; Background-related

(defun gen-random-star ()
  (list (random 960)
	(random 460)))

(defun gen-stars ()
  (let ((stars-amount (case *lod*
			((:low)  15)
			((:mid)  20)
			((:high) 30)
			(otherwise 15))))
  (loop for x from 0 to 2
     collect (loop for y from 0 to stars-amount collect (gen-random-star)))))


(defparameter *stars-positions* (gen-stars))
(defparameter *stars-parallax-centers* '(0 0 0))
(defparameter *stars-parallax-factors* '(0.5 0.8 1.1))

(defun update-stars (dt)
  (setf *stars-parallax-centers*
	(loop for center in *stars-parallax-centers*
	   for factor in *stars-parallax-factors*
	   collect (mod (- center (* factor dt)) (max-x)))))

(defun draw-stars ()
  (loop for layer in *stars-positions*
     for n from 0 to 2
     do (loop for star in layer
	   do (let ((position (list (mod (+ (nth n *stars-parallax-centers*)
					    (car star))
					 (max-x))
				    (cadr star))))
		(gl:with-pushed-matrix
		  (gsk-util:transform-translate position)
		  (gsk-util:no-stroke)
		  (gsk-util:with-fill-color '(255 255 255)
		    (gsk-util:ellipse '(0 0) '(5 5))))))))


;;; Enemy-related
(defstruct enemy
  type
  position
  angle
  health
  (alive t))

(defmethod update-troop (troop dt)
  (decf (car (enemy-position troop)) (* 0.5 dt)))

(defmethod update-enemy ((the-enemy enemy) dt)
  (case (enemy-type the-enemy)
    ((:troop) (update-troop the-enemy dt)))
  (when (< (car (enemy-position the-enemy)) 0)
    (setf (enemy-alive the-enemy) nil)))

(defmethod draw-enemy ((the-enemy enemy))
  (gl:with-pushed-matrix
    (gsk-util:transform-translate (enemy-position the-enemy))
    (gsk-util:transform-rotate (enemy-angle the-enemy))
    (gsk-util:transform-scale '(2.0 2.0))
    (case (enemy-type the-enemy)
      ((:troop)
       (gsk-util:with-stroke-color '(128 0 80)
	 (gsk-util:with-stroke-weight 6.0
	   (gsk-util:triangle '(0 -15) '(9 12) '(-9 12))))))))
     


;;; Level-related
(defparameter *level-layout* (make-hash-table))
(defparameter *level-size* 600)
(defparameter *level-step* 0)
(defparameter *level-pace* 100)
(defparameter *level-last-spawn-moment* 0)
(defparameter *level-position* 0)
(defparameter *onscreen-enemies* nil)

(defun spawn-enemy (type height)
  (push (make-enemy :type type
		    :position (list (+ (max-x) 50)
				    height)
		    :angle (/ (* pi 3.0) 2.0))
	*onscreen-enemies*))

(defun level-update (dt)
  (incf *level-step* dt)
  (when (> *level-step* *level-pace*)
    (setf *level-step* (mod *level-step*
			    *level-pace*))
    (incf *level-position*))
  (when (not (= *level-position* *level-last-spawn-moment*))
    (let ((spawn-data (gethash *level-position* *level-layout*)))
      (when spawn-data
	;; spawn stuff
	(loop for enemy in spawn-data
	   do (spawn-enemy (car enemy) (cadr enemy)))
	(setf *level-last-spawn-moment* *level-position*)))))


;;; Ship-related

(defparameter *ship-position* (from-center '(-400 0)))
(defparameter *ship-rotation* 0)
(defparameter *ship-max-speed* 1)
(defparameter *ship-current-gun* :normal)
(defparameter *ship-weapon-cooldown* 0)
(defparameter *ship-torpedo-cooldown* 500)
(defparameter *ship-health* 100)


;; Projectiles stuff

;; Keep a projectile pool so that we can keep memory control.
(defparameter *projectile-max* 10)
(defparameter *projectile-pool*
  (loop for x from 1 to *projectile-max*
     collect (cons :normal nil)))

;; Projectile types can be :normal and :torpedo, for now, each behaving
;; differently.
;; If cdr of a projectile is nil, then the projectile is not allocated;
;; if not, then the projectile exists.
;; The cdr of each projectile cell may also carry extra projectile
;; information, such as position, angle, etc.

(defstruct projectile-info
  position angle)

(defun shoot ()
  (loop named find-free-projectile
     for projectile in *projectile-pool*
     when (null (cdr projectile))
     do (setf (car projectile) *ship-current-gun*)
       (when (or (eq *ship-current-gun* :normal)
		(and (eq *ship-current-gun* :torpedo)
		   (equal *ship-weapon-cooldown* 0)))
	 (when (eq *ship-current-gun* :torpedo)
	   (setf *ship-weapon-cooldown* *ship-torpedo-cooldown*))
	 (setf (cdr projectile)
	       (make-projectile-info
		;; Position is the tip of the ship
		:position (list (+ (car *ship-position*) 9)
				(cadr *ship-position*))
		:angle (+ *ship-rotation* (/ pi 2.0)))))
       (return-from find-free-projectile)))

(defun update-projectiles (dt)
  ;; Projectiles are destroyed when out of play area on X axis.
  ;; They also wrap around on the Y axis, like the ship.
  (loop for projectile in *projectile-pool*
     when (not (null (cdr projectile)))
     do (let* ((projectile-type (car projectile))
	       (projectile-info (cdr projectile))
	       (projectile-position (projectile-info-position projectile-info))
	       (projectile-angle (projectile-info-angle projectile-info))
	       (projectile-speed (case projectile-type
				   ((:normal) 1.3)
				   ((:torpedo) 0.7))))
	  (if (> (car projectile-position) (max-x))
	      (setf (cdr projectile) nil)
	      (progn
		(setf (projectile-info-position (cdr projectile))
		      (list (+ (car projectile-position)
			       (* projectile-speed
				  (sin projectile-angle)
				  dt))
			    (mod (+ (cadr projectile-position)
				    (* projectile-speed
				       (- (cos projectile-angle))
				       dt))
				 (max-y)))))))
     ;; Collision detection
     ;; Compare projectile position for each enemy onscreen
       
       ))

(defun draw-projectiles ()
  (loop for projectile in *projectile-pool*
     when (not (null (cdr projectile)))
     do (let ((projectile-type (car projectile))
	      (projectile-position (projectile-info-position (cdr projectile)))
	      (projectile-angle (projectile-info-angle (cdr projectile))))
	  (gl:with-pushed-matrix
	       (gsk-util:transform-translate projectile-position)
	       (gsk-util:transform-rotate projectile-angle)
	       (case projectile-type
		 ((:normal)
		  (gsk-util:with-stroke-weight 1.0
		    (gsk-util:with-stroke-color '(255 0 0)
		      (gsk-util:with-fill-color '(128 0 90)
			(gsk-util:ellipse '(0 0) '(8 8))))))
		 ((:torpedo)
		  (gsk-util:with-stroke-weight 3.0
		    (gsk-util:with-stroke-color '(255 0 0)
		      (gsk-util:with-fill-color '(128 0 90)
			(gsk-util:rect '(-5 0) '(5 20)))))))))))


(defun update-ship (dt)
  ;; Rotate according to user input
  (setf *ship-rotation*
	(cond ((gsk-input:pressingp :up)
	       (- *ship-rotation* (/ dt 500.0)))
	      ((gsk-input:pressingp :down)
	       (+ *ship-rotation* (/ dt 500.0)))
	      ((<= (abs *ship-rotation*) 0.05)
	       0.0)
	      (t (+ *ship-rotation*
		    (* (/ dt 375.0) (- (signum *ship-rotation*)))))))
  ;; Clamp rotation between -quarterpi and quarterpi
  (setf *ship-rotation* (gsk-util:clamp *ship-rotation*
					:min (- (/ pi 4.0))
					:max (/ pi 4.0)))
  ;; Move around the screen according to rotation.
  (let ((y (cadr *ship-position*)))
    (when (not (= *ship-rotation* 0.0))
      (setf (cadr *ship-position*)
	    (+ y (* *ship-max-speed*
		    (/ *ship-rotation* (/ pi 4.0))
		    dt))))
    ;; I was using a `mod` form on the previous statement
    ;; to ensure a wraparound, but I figured I'd better do it
    ;; manually since the ship was snapping weirdly
    (when (< y 0) (setf (cadr *ship-position*) (max-y)))
    (when (> y (max-y)) (setf (cadr *ship-position*) 0)))
  ;; Shoot.
  ;; Check for key presses on normal gun (machinegun), and
  ;; single keypresses for torpedo.
  (when (case *ship-current-gun*
	  ((:normal) (gsk-input:pressingp :a))
	  ((:torpedo) (gsk-input:pressedp :a))
	  (otherwise nil))
    (shoot))
  ;; Replace weapon
  (when (gsk-input:pressedp :y)
    (setf *ship-current-gun*
	  (case *ship-current-gun*
	    ((:normal) :torpedo)
	    ((:torpedo) :normal))))
  ;; Weapon cooldown
  (setf *ship-weapon-cooldown*
	(max (- *ship-weapon-cooldown* dt) 0)))


(defun draw-ship ()
  (gl:with-pushed-matrix
    (gsk-util:transform-translate *ship-position*)
    (gsk-util:transform-rotate (+ *ship-rotation* (/ pi 2.0)))
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(255 255 255)
      (gsk-util:triangle '(0 -15) '(9 12) '(-9 12)))))


;;; HUD-related

(defun machinegun-icon ()
  (gl:with-pushed-matrix
    (gsk-util:transform-translate '(-10 -10))
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(128 0 90)
      ;; 1st row
      (gsk-util:rect '(0 5) '(3 2))
      (gsk-util:rect '(0 7) '(3 2))
      (gsk-util:rect '(0 9) '(3 2))
      ;; 2nd row
      (gsk-util:rect '(3 6) '(3 2))
      (gsk-util:rect '(3 8) '(3 2))
      ;; 3rd part
      (gsk-util:rect '(6 7) '(3 2)))))

(defun torpedo-icon ()
  (gl:with-pushed-matrix
    (gsk-util:transform-translate '(-10 -10))
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(128 0 90)
      (gsk-util:rect '(0 6) '(8 3))
      (gsk-util:triangle '(3 3) '(8 6) '(3 6))
      (gsk-util:triangle '(3 9) '(8 9) '(3 12))
      (gsk-util:arc '(8 7.5) '(3 3) (- (/ pi 2.0)) (/ pi 2.0)))))

(defun draw-hud ()
  ;; Debug level pace
  (gl:with-pushed-matrix
    (gsk-util:transform-translate '(10 20))
    (gsk-util:with-fill-color '(255 255 255)
      (gsk-util:no-stroke)
      (gsk-util:text-size 1.0)
      (gsk-util:text (format nil "levelposition: ~12,'0d" *level-position*)
		     '(0 0))))
  ;; Line
  (gl:with-pushed-matrix
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(255 255 255)
      (gsk-util:line (list 0 (max-y))
		     (list (max-x) (max-y)))))
  ;; Weapon type indicator
  (gl:with-pushed-matrix
    (gsk-util:transform-translate (list 100 (+ (max-y) 45)))
    (gsk-util:transform-scale '(5.0 4.5))
    (gsk-util:with-stroke-weight 3.0
      (case *ship-current-gun*
	((:normal) (machinegun-icon))
	((:torpedo) (torpedo-icon)))))
  ;; Weapon cooldown indicator
  (gl:with-pushed-matrix
    (gsk-util:transform-translate (list 40 (+ (max-y) 65)))
    (gsk-util:with-stroke-color '(255 0 0)
      (gsk-util:with-stroke-weight 5.0
	(gsk-util:line '(0 0)
		       (list (* 180 (/ *ship-weapon-cooldown*
				      *ship-torpedo-cooldown*))
			     0)))))
  ;; Weapon ammo indicator
  (when (not (eq *ship-current-gun* :normal))
    (gl:with-pushed-matrix
      (gsk-util:transform-translate (list 120 (+ (max-y) 35)))
      (gsk-util:no-stroke)
      (gsk-util:text-size 3.0)
      (gsk-util:text (format nil "x~3,'0d" 0) '(0 0))))
  ;; Ship health indicator
  (gl:with-pushed-matrix
      (gsk-util:transform-translate (list (- (/ (max-x) 2.0) 40.0)
					  (+ (max-y) 40)))
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(128 20 0)
      (gsk-util:with-stroke-weight 3.0
	(gsk-util:arc '(0 0)
		      '(50 50)
		      0
		      (/ (* (* 2.0 pi)
			    *ship-health*)
			 100.0))))
    (gsk-util:with-stroke-color '(255 255 255)
      (gsk-util:text-size 1.0)
      (gsk-util:text "Health" '(-20 0))))
  ;; Score indicator
  (gl:with-pushed-matrix
    (gsk-util:transform-translate (list (- (max-x) 320)
					(+ (max-y) 25)))
    (gsk-util:with-fill-color '(255 255 255)
      (gsk-util:no-stroke)
      (gsk-util:text-size 2.0)
      ;; Score
      (gsk-util:text (format nil "Score: ~12,'0d" *score*)
		     '(0 0))
      ;; High-score
      (gsk-util:transform-translate '(0 30))
      (gsk-util:text (format nil "High:  ~12,'0d" *high*)
		     '(0 0)))))

(defun setup ()
  (gsk-util:set-font-texture "../res/gohufont.png" '(8 13))
  (spawn-enemy :troop 300))

(defun update (dt)
  (when (gsk-input:pressedp :start)
    (setf *paused* (not *paused*)))
  (when (gsk-input:pressedp :b)
    (restart-game))
  (when (not *paused*)
    (update-stars dt)
    (update-ship dt)
    (update-projectiles dt)
    (level-update dt)
    (loop for enemy in *onscreen-enemies*
       do (update-enemy enemy dt))
    (setf *onscreen-enemies*
	  (remove-if (lambda (enemy)
		       (eq (enemy-alive enemy) nil))
		     *onscreen-enemies*))))

(defun draw ()
  (draw-stars)
  (draw-ship)
  (draw-hud)
  (draw-projectiles)
  (loop for enemy in *onscreen-enemies*
       do (draw-enemy enemy)))


(defun load-placeholder-level ()
  (setf *level-layout* (make-hash-table))
  (loop for x from 0 to 9
     do (let ((base-time (* x 40)))
	  (setf (gethash base-time *level-layout*)
		'((:troop 100)
		  (:troop 250)
		  (:troop 400)))
	  (setf (gethash (+ base-time 20) *level-layout*)
		'((:troop 175)
		  (:troop 325))))))
  
				      


(defun restart-game ()
  (gsk:next-frame
    ;; Reset level
    (when (> *score* *high*)
      (setf *high* *score*))
    (setf *level-position* 0)
    (setf *level-last-spawn-moment* 0)
    (setf *onscreen-enemies* nil)
    ;; TODO: reload level 0
    ;; for now we're loading the placeholder level.
    (load-placeholder-level)
    ;; Reset ship specs
    (setf *ship-current-gun* :normal)
    (setf *ship-position* (from-center '(-400 0)))
    (setf *ship-weapon-cooldown* 0)
    (setf *ship-health* 100)
    (setf *projectile-pool*
	  (loop for x from 1 to *projectile-max*
	     collect (cons :normal nil)))))
    

(gsk:add-setup-callback 'setup)
(gsk:add-update-callback 'update)
(gsk:add-draw-callback 'draw)

;;(gsk:run-sketch)

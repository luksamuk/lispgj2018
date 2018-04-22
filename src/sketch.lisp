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

(defmacro max-x ()
  `(cdr (car *boundaries*)))

(defmacro max-y ()
  `(cdr (cadr *boundaries*)))

;;; Background-related

(defun gen-random-star ()
  (list (random 960)
	(random 460)))


(defparameter *stars-positions*
  (let ((stars-amount (case *lod*
			((:low)  15)
			((:mid)  20)
			((:high) 30)
			(otherwise 15))))
  (loop for x from 0 to 2
     collect (loop for y from 0 to stars-amount collect (gen-random-star)))))
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

;;; Ship-related

(defparameter *ship-position* (from-center '(-400 0)))
(defparameter *ship-rotation* 0)
(defparameter *ship-max-speed* 1)

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
    (when (> y (max-y)) (setf (cadr *ship-position*) 0))))


(defun draw-ship ()
  (gl:with-pushed-matrix
    (gsk-util:transform-translate *ship-position*)
    (gsk-util:transform-rotate (+ *ship-rotation* (/ pi 2.0)))
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(255 255 255)
      (gsk-util:triangle '(0 -15) '(9 12) '(-9 12)))))


;;; HUD-related

(defun draw-hud ()
  (gl:with-pushed-matrix
    (gsk-util:no-fill)
    (gsk-util:with-stroke-color '(255 255 255)
      (gsk-util:line (list 0 (max-y))
		     (list (max-x) (max-y))))))
     

(defun update (dt)
  (update-stars dt)
  (update-ship dt))

(defun draw ()
  (draw-stars)
  (draw-ship)
  (draw-hud))

(gsk:add-update-callback 'update)
(gsk:add-draw-callback 'draw)

;;(gsk:run-sketch)

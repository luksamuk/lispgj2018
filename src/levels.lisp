;;;; levels.lisp
;;;; Part of ORBIT DEFENSE STRIKEFORCE V0.1
;;;; Copyright © 2018 Lucas Vieira
;;;;
;;;; This code is distributed under the MIT License.
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defmacro enemy-spawn (time enemy-list)
  `(setf (gethash ,time *level-layout*) ,enemy-list))

(defmacro with-new-level (&body body)
  `(setf *level-layout* (make-hash-table))
  `(progn ,@body))

;; Duration: 30
(defun spawn-delta-formation (start-time y-center)
  (enemy-spawn start-time (list (list :troop y-center)))
  (enemy-spawn (+ start-time 5)
	       (list (list :troop (- y-center 50))
		     (list :troop (+ y-center 50))))
  (enemy-spawn (+ start-time 10)
	       (list (list :troop (- y-center 100))
		     (list :troop (+ y-center 100)))))


(defun load-level-1 ()
  (with-new-level
    (loop for x from 0 to 5
       do (let ((base-time (* x 40)))
	    (enemy-spawn base-time '((:troop 100)
				     (:troop 250)
				     (:troop 400)))
	    (enemy-spawn (+ base-time 20) '((:troop 175)
					    (:troop 325)))))
    ;; Delta formations
    (spawn-delta-formation 260 230)
    (spawn-delta-formation 290 130)
    (spawn-delta-formation 320 330)
    ;; Repeat in reverse
    (spawn-delta-formation 380 330)
    (spawn-delta-formation 410 130)
    (spawn-delta-formation 440 230)
    ;; Let's try something
    (spawn-delta-formation 460 130)
    (spawn-delta-formation 475 330)
    (spawn-delta-formation 490 130)
    (spawn-delta-formation 505 330)))
    

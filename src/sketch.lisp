(ql:quickload 'game-sketch)

(defun update (dt)
  (declare (ignore dt)))

(defun draw () )

(gsk:add-update-callback 'update)
(gsk:add-draw-callback 'draw)

;;(gsk:run-sketch)

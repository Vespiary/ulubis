
(in-package :ulubis.wmii)

(defparameter *default-mode* nil)
(defparameter *ortho* (m4:identity))

(defmode wmii-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))
   (projection :accessor projection :initarg :projection :initform (m4:identity))
   (focus-follows-mouse :accessor focus-follows-mouse :initarg :focus-follows-mouse :initform nil)))

(defmethod init-mode ((mode wmii-mode))
  (cepl:map-g #'mapping-pipeline nil)
  (setf (render-needed *compositor*) t))

(defun pointer-changed-surface (mode x y old-surface new-surface)
  (setf (cursor-surface *compositor*) nil)
  ;; (format t "Pointer changed service~%")
  (when (focus-follows-mouse mode)
    (deactivate old-surface)) 
  (when (and old-surface (pointer (client old-surface)))
    (wl-pointer-send-leave (->resource (pointer (client old-surface)))
			   0
			   (->resource (wl-surface old-surface))))
  (setf (pointer-surface *compositor*) new-surface)
  (when (focus-follows-mouse mode)
    (activate-surface new-surface mode))
  (when (and new-surface (pointer (client new-surface)))
    (wl-pointer-send-enter (->resource (pointer (client new-surface)))
			   0
			   (->resource (wl-surface new-surface))
			   (round (* 256 (- x (x new-surface))))
			   (round (* 256 (- y (y new-surface)))))))

(defun send-surface-pointer-motion (x y time surface)
  (when (and surface (pointer (client surface)))
    (wl-pointer-send-motion (->resource (pointer (client surface)))
			    time
			    (round (* 256 (- x (x surface))))
			    (round (* 256 (- y (y surface)))))
    ;; Need to check client handles version 5
    ;;(wl-pointer-send-frame (waylisp:->pointer (waylisp:client surface)))
    ))

(defun update-pointer (delta-x delta-y)
  (with-slots (pointer-x pointer-y screen-width screen-height) *compositor*
    (incf pointer-x delta-x)
    (incf pointer-y delta-y)
    (when (< pointer-x 0) (setf pointer-x 0))
    (when (< pointer-y 0) (setf pointer-y 0))
    (when (> pointer-x screen-width) (setf pointer-x screen-width))
    (when (> pointer-y screen-height) (setf pointer-y screen-height))))

(defmethod mouse-motion-handler ((mode wmii-mode) time delta-x delta-y)
  ;; Update the pointer location
  (with-slots (pointer-x pointer-y) *compositor*
    (update-pointer delta-x delta-y)
    (when (cursor-surface *compositor*)
      (setf (render-needed *compositor*) t))
    (let ((old-surface (pointer-surface *compositor*))
	  (current-surface (surface-under-pointer pointer-x pointer-y (view mode))))
      (cond
	;; 1. The pointer has left the current surface
	((not (equalp old-surface current-surface))
	 (setf (cursor-surface *compositor*) nil)
	 (pointer-changed-surface mode pointer-x pointer-y old-surface current-surface))
	;; 2. Pointer is over previous surface
	((equalp old-surface current-surface)
	 (send-surface-pointer-motion pointer-x pointer-y time current-surface))))))

(defmethod mouse-button-handler ((mode wmii-mode) time button state)
  ;; 1. Send active surface mouse button
  (when (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       (view mode)) 
    (let ((surface (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       (view mode)) ))
      (when (and surface (pointer (client surface)))
	(wl-pointer-send-button (->resource (pointer (client surface)))
				0
				time
				button
				state)))))

(defmethod first-commit ((mode wmii-mode) (surface isurface))
  (setf (origin-x surface) 0)
  (setf (origin-y surface) 0)
  (activate-surface surface mode))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (apply #'gl:clear-color (clear-color mode))
  (let ((*ortho* (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (mapcar (lambda (surface)
                (render surface view-fbo))
              (reverse (surfaces (view mode)))))))

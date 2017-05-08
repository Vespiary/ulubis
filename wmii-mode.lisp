
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

(defun move-surface (x y move-op)
  "Move surface to location X and Y given the MOVE-OP"
  (let ((surface (move-op-surface move-op)))
    (setf (x surface) (round (+ (move-op-surface-x move-op) (- x (move-op-pointer-x move-op)))))
    (setf (y surface) (round (+ (move-op-surface-y move-op) (- y (move-op-pointer-y move-op)))))
    (setf (render-needed *compositor*) t)))

(defun resize-surface (x y view resize-op)
  "Resize surface given new pointer location (X,Y) and saved information in RESIZE-OP"
  (let* ((saved-width (resize-op-surface-width resize-op))
	 (saved-height (resize-op-surface-height resize-op))
	 (saved-pointer-x (resize-op-pointer-x resize-op))
	 (saved-pointer-y (resize-op-pointer-y resize-op))
	 (delta-x (- x saved-pointer-x))
	 (delta-y (- y saved-pointer-y)))
    (resize-surface-absolute (resize-op-surface resize-op) view (+ saved-width delta-x) (+ saved-height delta-y))))

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

(cepl:defun-g wmii-mode-vertex-shader ((vert cepl:g-pt) &uniform (origin :mat4) (origin-inverse :mat4) (surface-scale :mat4) (surface-translate :mat4))
  (values (* *ortho* surface-translate origin-inverse surface-scale origin (cepl:v! (cepl:pos vert) 1))
	  (:smooth (cepl:tex vert))))

(cepl:def-g-> mapping-pipeline ()
  (desktop-mode-vertex-shader cepl:g-pt) (default-fragment-shader :vec2))

(defmethod render ((surface isurface) &optional view-fbo)
  (when (texture (wl-surface surface))
    (with-rect (vertex-stream (width (wl-surface surface)) (height (wl-surface surface)))
      (let ((texture (texture-of surface)))
	(gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
	(map-g-default/fbo view-fbo #'mapping-pipeline vertex-stream
			   :origin (m4:translation (cepl:v! (- (origin-x surface)) (- (origin-y surface)) 0))
			   :origin-inverse (m4:translation (cepl:v! (origin-x surface) (origin-y surface) 0))
			   :surface-scale (m4:scale (cepl:v! (scale-x surface) (scale-y surface) 1.0))
			   :surface-translate (m4:translation (cepl:v! (x surface) (y surface) 0.0))
			   :texture texture
			   :alpha (opacity surface))))
    (loop :for subsurface :in (reverse (subsurfaces (wl-surface surface)))
       :do (render subsurface view-fbo))))

(defmethod render ((surface wl-subsurface) &optional view-fbo)
  (when (texture (wl-surface surface))
    (with-rect (vertex-stream (width (wl-surface surface)) (height (wl-surface surface)))
      (let ((texture (texture-of surface)))
	(gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
	(map-g-default/fbo view-fbo #'mapping-pipeline vertex-stream
			   :origin (m4:translation (cepl:v! (+ (x surface) (- (origin-x (role (parent surface)))))
							    (+ (y surface) (- (origin-y (role (parent surface)))))
							    0))
			   :origin-inverse (m4:translation (cepl:v! (+ (- (x surface)) (origin-x (role (parent surface))))
								    (+ (- (y surface)) (origin-y (role (parent surface))))
								    0))
			   :surface-scale (m4:scale (cepl:v! (scale-x (role (parent surface)))
							     (scale-y (role (parent surface)))
							     1.0))
			   :surface-translate (m4:translation (cepl:v! (+ (x (role (parent surface))) (x surface))
								       (+ (y (role (parent surface))) (y surface))
								       0.0))
			   :texture texture
			   :alpha (opacity surface))))
    (loop :for subsurface :in (reverse (subsurfaces (wl-surface surface)))
       :do (render subsurface view-fbo))))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (apply #'gl:clear-color (clear-color mode))
  (let ((*ortho* (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (mapcar (lambda (surface)
                (render surface view-fbo))
              (reverse (surfaces (view mode)))))))

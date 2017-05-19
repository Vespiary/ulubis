
(in-package :ulubis)

(defmode desktop-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))))

(defun move-surface (x y move-op)
  "Move surface to location X and Y given the MOVE-OP"
  (let ((surface (move-op-surface move-op)))
    (setf (x surface) (round (+ (move-op-surface-x move-op) (- x (move-op-pointer-x move-op)))))
    (setf (y surface) (round (+ (move-op-surface-y move-op) (- y (move-op-pointer-y move-op)))))
    (request-render)))

(defun resize-surface (x y view resize-op)
  "Resize surface given new pointer location (X,Y) and saved information in RESIZE-OP"
  (let* ((saved-width (resize-op-surface-width resize-op))
	 (saved-height (resize-op-surface-height resize-op))
	 (saved-pointer-x (resize-op-pointer-x resize-op))
	 (saved-pointer-y (resize-op-pointer-y resize-op))
	 (delta-x (- x saved-pointer-x))
	 (delta-y (- y saved-pointer-y)))
    (resize-surface-absolute (resize-op-surface resize-op) view (+ saved-width delta-x) (+ saved-height delta-y))))

(defun pulse-animation (surface)
  (setf (origin-x surface) (/ (width (wl-surface surface)) 2))
  (setf (origin-y surface) (/ (height (wl-surface surface)) 2))
  (sequential-animation nil
			(parallel-animation nil
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.05 :target surface :property 'scale-x)
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.05 :target surface :property 'scale-y))
			(parallel-animation nil
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.0 :target surface :property 'scale-x)
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.0 :target surface :property 'scale-y))))

(defmethod mouse-button-handler ((mode desktop-mode) time button state)
  ;; 1. Change (possibly) the active surface
  (when (and (= button #x110) (= state 1) (= 0 (mods-depressed (mods *compositor*))))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      ;; When we click on a client which isn't the first client
      (when (and surface (not (equalp surface (active-surface (view mode)))))
	(start-animation (pulse-animation surface) :finished-fn (lambda ()
								  (setf (origin-x surface) 0.0)
								  (setf (origin-y surface) 0.0))))
      (activate-surface surface mode)
      (when surface
	(raise-surface surface (view mode))
	(request-render))))
  
  ;; Drag window
  (when (and (= button #x110) (= state 1) (= Gui (mods-depressed (mods *compositor*))))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      (when surface
	(setf (moving-surface *compositor*) ;;surface))))
	      (make-move-op :surface surface
			    :surface-x (x surface)
			    :surface-y (y surface)
			    :pointer-x (pointer-x *compositor*)
			    :pointer-y (pointer-y *compositor*))))))
	      
  ;; stop drag
  (when (and (moving-surface *compositor*) (= button #x110) (= state 0))
    (setf (moving-surface *compositor*) nil))

  ;; Resize window
  (when (and (= button #x110) (= state 1) (= (+ Gui Shift) (mods-depressed (mods *compositor*))))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      (let ((width (if (input-region (wl-surface surface))
		       (width (first (last (rects (input-region (wl-surface surface))))))
		       (width (wl-surface surface))))
	    (height (if (input-region (wl-surface surface))
			(height (first (last (rects (input-region (wl-surface surface))))))
			(height (wl-surface surface)))))
	(setf (resizing-surface *compositor*) (make-resize-op :surface surface
							      :pointer-x (pointer-x *compositor*)
							      :pointer-y (pointer-y *compositor*)
							      :surface-width width
							      :surface-height height)))))

  (when (and (resizing-surface *compositor*) (= button #x110) (= state 0))
    (setf (resizing-surface *compositor*) nil))
  
  ;; 2. Send active surface mouse button
  (call-next-method))

(defkeybinding (:pressed "Tab" Gui) (mode) (desktop-mode)
  (push-mode (view mode) (make-instance 'alt-tab-mode)))

(defmethod first-commit ((mode desktop-mode) (surface isurface))
  (let ((animation (sequential-animation
		    (lambda ()
		      (setf (origin-x surface) 0.0)
		      (setf (origin-y surface) 0.0))
		    (animation :target surface
			       :property 'scale-x
			       :easing-fn 'easing:out-exp
			       :from 0
			       :to 1.0
			       :duration 250)
		    (animation :target surface
			       :property 'scale-y
			       :easing-fn 'easing:out-exp
			       :to 1.0
			       :duration 250))))
    (setf (origin-x surface) (/ (width (wl-surface surface)) 2))
    (setf (origin-y surface) (/ (height (wl-surface surface)) 2))
    (setf (scale-y surface) (/ 6 (height (wl-surface surface))))
    (setf (first-commit-animation surface) animation)
    (start-animation animation)))

(defmethod render ((mode desktop-mode) &optional view-fbo)
  (let ((*ortho* (make-ortho 0 (desktop-width) (desktop-height) 0 1 -1)))
    (apply #'gl:clear-color (clear-color mode))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (mapcar (lambda (surface)
                (render surface view-fbo))
              (reverse (surfaces (view mode)))))))

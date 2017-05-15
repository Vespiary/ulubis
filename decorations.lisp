
(in-package :ulubis)

(defclass decorated-surface ()
  ((surface :accessor surface :initarg :surface :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform nil)
   (x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (width :accessor width :initarg :width :initform 1)
   (height :accessor height :initarg :height :initform 1)
   (state :accessor state :initarg :state :initform t :documentation "t - render everything, :borderless - just the underlying surface, :folded - just the decorations")))

(defmethod (setf x) :after (new-x (instance decorated-surface))
  (setf (x decoration) new-x)
  (setf (x (surface instance))
        (+ new-x (border (decoration instance)))))

(defmethod (setf y) :after (new-y (instance decorated-surface))
  (setf (y decoration) new-y)
  (setf (y (surface instance))
        (+ new-y (header (decoration instance)))))

(defmethod resize ((instance decorated-surface) width height time &key activate?)
  (setf (width instance) width)
  (setf (height instance) height)
  (setf (width (decoration instance)) width)
  (setf (height (decoration instance)) height)
  (resize-decoration (width instance) (height instance))
  (multiple-value-bind (width height)
      (underlying-surface-size instance)
    (resize (surface instance) width height time :activate activate?)))

(defun underlying-surface-size (instance)
  (case (state instance)
    (:borderless (values (width instance) (height instance)))
    (:folded (values 0 0))
    (t (values
        (- width (* 2 (border (decoration instance))))
        (- height (+ (border (decoration instance)) (header (decoration instance))))))))

(defclass decoration ()
  ((header :accessor header :initarg :header :initform 10 :documentation "Header height in pixels")
   (border :accessor border :initarg :border :initform 0 :documentation "Border thickness in pixels")
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (width :accessor width :initform 1)
   (height :accessor height :initform 1)))

(defgeneric resize-decoration (decoration width height))
(defgeneric redraw-decoration (decoration))

(defmethod resize-decoration ((decoration decoration) width height)
  (redraw-decoration decoration))

;;; Rendering

(defmethod render ((instance decorated-surface) &optional view-fbo)
  (case (state instance)
    (:borderless (render (surface instance) view-fbo))
    (:folded (render (decoration instance) view-fbo))
    (t
     (render (decoration instance) view-fbo)
     (render-window-geometry-region (surface instance) view-fbo))))

(defmethod render ((instance decoration) &optional view-fbo)
  (with-slots (x y width height) instance
    (let* ((array (cepl:make-gpu-array (make-g-pt-quad y
                                                       (+ y height)
                                                       x
                                                       (+ x width))
                                       :element-type 'cepl:g-pt))
           (vertex-stream (cepl:make-buffer-stream array)))
      (map-g-default/fbo view-fbo #'cursor-pipeline vertex-stream
                         :ortho (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)
                         :texture (texture-of instance))
      (cepl:free vertex-stream)
      (cepl:free array))))

(defun render-window-geometry-region (surface &optional view-fbo)
  "Renders only window's geometry throwing away the shadows etc"
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

;;; Dummy implementation

(defclass empty-decoration (decoration cairo-surface)
  ())

(defmethod initialize-instance :after ((instance empty-decoration) &key)
  (setf (header instance) 0)
  (setf (draw-func instance)
        (lambda ()
          (cl-cairo2:set-source-rgba 1 1 1 0)
          (cl-cairo2:paint))))

(defmethod resize-decoration ((decoration empty-decoration) width height)
  (cairo-surface-resize decoration width height)
  (call-next-method))

(defmethod redraw-decoration ((decoration empty-decoration))
  (cairo-surface-redraw decoration))


(in-package :ulubis)

(declaim (optimize (debug 3)))

(defclass decorated-surface ()
  ((surface :accessor surface :initarg :surface :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform nil)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (state :accessor state :initarg :state :initform t :documentation "t - render everything, :borderless - just the underlying surface, :folded - just the decorations")))

(defmethod initialize-instance :after ((instance decorated-surface) &key)
  (with-slots (surface decoration x y width height) instance
    (multiple-value-bind (top bottom left right) (decoration-padding (decoration instance))
      (let ((geometry (window-geometry surface)))
        (setf x (+ (x surface) (window-geometry-x geometry) (- left)))
        (setf y (+ (y surface) (window-geometry-y geometry) (- top)))
        (setf width (+ (window-geometry-w geometry) (+ left right)))
        (setf height (+ (window-geometry-h geometry) (+ top bottom)))
        (setf (surface decoration) surface)
        (setf (x decoration) x)
        (setf (y decoration) y)
        (resize-decoration decoration width height)))))

(defmethod (setf x) :after (new-x (instance decorated-surface))
  (setf (x (decoration instance)) new-x)
  (update-decorated-position instance :new-x new-x))

(defmethod (setf y) :after (new-y (instance decorated-surface))
  (setf (y (decoration instance)) new-y)
  (update-decorated-position instance :new-y new-y))

(defmethod resize ((instance decorated-surface) width height time &key activate? maximize fullscreen)
  (with-slots (decoration) instance
    (setf (width instance) width)
    (setf (height instance) height)
    (resize-decoration decoration (width instance) (height instance))
    (multiple-value-bind (width height)
        (underlying-surface-size instance)
      (resize (surface instance) width height time
              :activate? activate? :maximize maximize :fullscreen fullscreen))))

(defun underlying-surface-size (instance)
  (with-slots (width height) instance
    (case (state instance)
      (:borderless (values width height))
      (:folded (values 0 0))
      (t (multiple-value-bind (top bottom left right)
             (decoration-padding (decoration instance))
           (values
            (- width left right)
            (- height top bottom)))))))

(defun update-decorated-position (decorated &key new-x new-y)
  (unless new-x
    (setf new-x (x decorated)))
  (unless new-y
    (setf new-y (y decorated)))
  (with-slots (decoration surface) decorated
    (multiple-value-bind (top bottom left right) (decoration-padding decoration)
      (declare (ignore bottom right))
      (setf (y surface)
            (+ new-y top
               (- (window-geometry-y (window-geometry surface)))))
      (setf (x surface)
            (+ new-x left
               (- (window-geometry-x (window-geometry surface))))))))

(defclass decoration ()
  ((x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (width :reader width :initform 1)
   (height :reader height :initform 1)
   (surface :accessor surface :initarg :surface :initform nil)))

(defgeneric decoration-padding (decoration)
  (:documentation "Must return four values which are top, bottom, left, and right padding of the client region in the decoration"))
(defgeneric resize-decoration (decoration width height))
(defgeneric redraw-decoration (decoration))

(defmethod resize-decoration ((decoration decoration) width height)
  (setf (slot-value decoration 'width) width)
  (setf (slot-value decoration 'height) height)
  (redraw-decoration decoration))

;;; Rendering

(cepl:defun-g culling-fragment-shader ((tex-coord :vec2)
                                       &uniform (top-left :vec2) (bottom-right :vec2) (texture :sampler-2d) (alpha :float))
  (if (and (>= (cepl:s~ varjo::gl-frag-coord :x) (cepl:s~ top-left :x))
           (<= (cepl:s~ varjo::gl-frag-coord :x) (cepl:s~ bottom-right :x))
           (>= (cepl:s~ varjo::gl-frag-coord :y) (cepl:s~ top-left :y))
           (<= (cepl:s~ varjo::gl-frag-coord :y) (cepl:s~ bottom-right :y)))
      (cepl:v! (cepl:s~ (cepl:texture texture tex-coord) :x)
               (cepl:s~ (cepl:texture texture tex-coord) :y)
               (cepl:s~ (cepl:texture texture tex-coord) :z)
               (* alpha (cepl:s~ (cepl:texture texture tex-coord) :w)))
      (cepl:v! 1 1 1 0)))

(cepl:def-g-> decoration-pipeline ()
  (mode-vertex-shader cepl:g-pt) (culling-fragment-shader :vec2))

(defmethod render ((instance decorated-surface) &optional view-fbo)
  (case (state instance)
    (:borderless (render (surface instance) view-fbo))
    (:folded (render (decoration instance) view-fbo))
    (nil nil)
    (t (render (decoration instance) view-fbo)
       (multiple-value-bind (width height) (underlying-surface-size instance)
         (multiple-value-bind (top bottom left right) (decoration-padding (decoration instance))
           (render-decorated-underlying-window (surface instance)
                                               (list (+ (y instance) top)
                                                     (+ (y instance) top height (- bottom))
                                                     (+ (x instance) left)
                                                     (+ (x instance) left width (- right)))
                                               view-fbo))))))

(defmethod render ((instance decoration) &optional view-fbo)
  (with-slots (x y width height) instance
    (with-rect (vertex-stream width height)
      (let ((texture (texture-of instance)))
        (gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
        (map-g-default/fbo view-fbo #'mapping-pipeline vertex-stream
                           :origin (m4:translation (cepl:v! 0 0 0))
                           :origin-inverse (m4:translation (cepl:v! 0 0 0))
                           :surface-scale (m4:scale (cepl:v! 1 1 1.0))
                           :surface-translate (m4:translation (cepl:v! x y 0.0))
                           :texture texture
                           :alpha 1.0)))))

(defun render-decorated-underlying-window (surface client-region &optional view-fbo)
  "Renders only window's geometry throwing away the shadows etc"
  (when (texture (wl-surface surface))
    (with-rect (vertex-stream (width (wl-surface surface)) (height (wl-surface surface)))
      (let ((texture (texture-of surface)))
        (gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
        (destructuring-bind (top bottom left right) client-region
          (map-g-default/fbo view-fbo #'decoration-pipeline vertex-stream
                             :origin (m4:translation (cepl:v! (- (origin-x surface)) (- (origin-y surface)) 0))
                             :origin-inverse (m4:translation (cepl:v! (origin-x surface) (origin-y surface) 0))
                             :surface-scale (m4:scale (cepl:v! (scale-x surface) (scale-y surface) 1.0))
                             :surface-translate (m4:translation (cepl:v! (x surface) (y surface) 0.0))
                             :texture texture
                             :top-left (cepl:v! left
                                                (- (screen-height *compositor*) bottom))
                             :bottom-right (cepl:v! right
                                                    (- (screen-height *compositor*) top))
                             :alpha (opacity surface)))))
    (loop :for subsurface :in (reverse (subsurfaces (wl-surface surface)))
       :do (render subsurface view-fbo))))

;;; Dummy implementation

(defclass empty-decoration (decoration cairo-surface)
  ())

(defmethod initialize-instance :after ((instance empty-decoration) &key)
  (setf (draw-func instance)
        (lambda ()
          (cl-cairo2:set-source-rgba 1 1 1 0)
          (cl-cairo2:paint))))

(defmethod decoration-padding ((decoration empty-decoration))
  (values 0 0 0 0))

(defmethod resize-decoration ((decoration empty-decoration) width height)
  (cairo-surface-resize decoration width height)
  (call-next-method))

(defmethod redraw-decoration ((decoration empty-decoration))
  (cairo-surface-redraw decoration))

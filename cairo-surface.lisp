
(in-package :ulubis)

(declaim (optimize (debug 3)))

(defclass cairo-surface ()
  ((cairo-surface :reader cairo-surface :initform nil)
   (cairo-context :reader cairo-context :initform nil)
   (gl-texture :reader gl-texture :initform nil)
   (gl-texture-up-to-date :initform nil)
   (draw-func :accessor draw-func :initarg :draw-func :initform (lambda ()))))

(defmethod initialize-instance :after ((instance cairo-surface) &key width height filename)
  (with-slots (cairo-surface cairo-context) instance
    (when (or width height filename)
      (if filename
          (if (or width height)
              (error "Need to specify either :filename or :width and :height")
              (setf cairo-surface (cl-cairo2:image-surface-create-from-png filename)))
          (if (and width height)
              (setf cairo-surface (cl-cairo2:create-image-surface :argb32 (truncate width) (truncate height)))
              (error "Need to specify either :filename or :width and :height")))
      (setf cairo-context (cl-cairo2:create-context cairo-surface))))
  (trivial-garbage:finalize instance #'finalize-instance))

(defun cairo-surface-resize (instance width height)
  "Creates a new cairo-surface of specified size and immediately calls redraw"
  (with-slots (cairo-surface cairo-context gl-texture) instance
    (when cairo-surface
      (cl-cairo2:destroy cairo-context)
      (cl-cairo2:destroy cairo-surface))
    (setf cairo-surface (cl-cairo2:create-image-surface :argb32 width height))
    (setf cairo-context (cl-cairo2:create-context cairo-surface))
    (when gl-texture
      (cepl:free gl-texture)
      (setf gl-texture nil))
    (cairo-surface-redraw instance)))

(defgeneric finalize-instance (instance))
(defmethod finalize-instance ((instance cairo-surface))
  (with-slots (cairo-surface gl-texture) instance
    (cl-cairo2:destroy cairo-surface)
    (when gl-texture
      (cepl:free gl-texture))))

(defmethod width ((instance cairo-surface))
  (cl-cairo2:width (cairo-surface instance)))

(defmethod height ((instance cairo-surface))
  (cl-cairo2:height (cairo-surface instance)))

(defgeneric cairo-surface-redraw (instance &optional custom-draw-func)
  (:documentation "Calls DRAW-FUNC to update surface pixels.
The call itself doesn't upload pixels to GPU, so it can be safely
called more often than TEXTURE-OF"))

(defun ensure-surface (cairo-surface)
  (unless (cairo-surface cairo-surface)
      (error "Surface not initialized. Pass :WIDTH, :HEIGHT or :FILENAME to MAKE-INSTANCE, or use CAIRO-SURFACE-RESIZE.")))

(defmethod cairo-surface-redraw ((instance cairo-surface) &optional custom-draw-func)
  (ensure-surface instance)
  (with-slots (cairo-surface cairo-context gl-texture-up-to-date) instance
    (let ((cl-cairo2:*surface* cairo-surface)
          (cl-cairo2:*context* cairo-context))
      (cl-cairo2:reset-trans-matrix)
      (if custom-draw-func
          (funcall custom-draw-func)
          (funcall (draw-func instance))))
    (setf gl-texture-up-to-date nil)))

(defmethod texture-of ((instance cairo-surface))
  (ensure-surface instance)
  (with-slots (cairo-surface gl-texture gl-texture-up-to-date) instance
    (unless gl-texture-up-to-date
      (cl-cairo2:surface-flush cairo-surface)
      (let* ((cairo-data (cl-cairo2:image-surface-get-data cairo-surface :pointer-only t))
             (cepl-data (cepl:make-c-array-from-pointer (list (width instance)
                                                              (height instance))
                                                        :uint8-vec4
                                                        cairo-data)))
        (if gl-texture
            (cepl:push-g cepl-data gl-texture)
            (setf gl-texture (cepl:make-texture cepl-data
                                                :pixel-format (cepl.types::make-pixel-format
                                                               :components :bgra
                                                               :type :uint8
                                                               :normalize t
                                                               :sizes nil
                                                               :reversed t
                                                               :comp-length 4))))
        (setf gl-texture-up-to-date t)))
    (cepl:sample gl-texture)))


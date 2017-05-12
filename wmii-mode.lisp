
(in-package :ulubis.wmii)

(defmode wmii-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))))

(defmethod init-mode ((mode wmii-mode))
  (cepl:map-g #'mapping-pipeline nil)
  (setf (render-needed *compositor*) t))

(defmethod first-commit ((mode wmii-mode) (surface isurface))
  (setf (origin-x surface) 0)
  (setf (origin-y surface) 0)
  (activate-surface surface mode))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (apply #'gl:clear-color (clear-color mode))
  (when view-fbo
    (cepl:clear view-fbo))
  (cepl:with-blending (blending-parameters mode)
    (mapcar (lambda (surface)
              (render surface view-fbo))
            (reverse (surfaces (view mode))))))

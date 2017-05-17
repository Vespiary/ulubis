
(in-package :ulubis)

(declaim (optimize (debug 3)))

(defmode wmii-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))
   (surfaces :accessor surfaces :initform nil)
   (layout :accessor layout :initarg :layout :initform nil)
   (fullscreen-surface :accessor fullscreen-surface :initform nil)))

(defmethod initialize-instance :after ((mode wmii-mode) &key)
  (setf (layout mode)
        (make-instance 'ulubis.wmii:layout
                       :width (screen-width *compositor*)
                       :height (screen-width *compositor*)
                       :reposition-callback (lambda (surface top bottom left right &rest keys)
                                              (apply #'wmii-reposition-window
                                                     mode surface top bottom left right keys))
                       :activate-callback (lambda (surface)
                                            (activate-surface (surface surface) mode)))))

(defun find-decorated (mode surface)
  (find surface (surfaces mode) :key #'surface))

(defmethod remove-surface-from-mode ((mode wmii-mode) (surface zxdg-toplevel-v6))
  (with-slots (surfaces) mode
    (let ((decorated (find-decorated mode surface)))
      (when decorated
        (ulubis.wmii:remove-surface decorated (layout mode))
        (setf surfaces (remove decorated surfaces))))))

(defmethod allow-move? ((mode wmii-mode) surface)
  "Allow moving of unmanaged windows"
  (not (member surface (ulubis.wmii:surfaces (layout mode))
               :key #'surface)))

(defun wmii-reposition-window (mode surface top bottom left right
                               &key (render-surface t) (visible t) fullscreen tabs)
  (unless visible
    (setf (state surface) nil)
    (return-from wmii-reposition-window))
  (setf (x surface) left)
  (setf (y surface) top)
  (cond (fullscreen
         (setf (state surface) :borderless)
         (setf (fullscreen-surface mode) surface))
        (t
         (setf (state surface) t)
         (setf (fullscreen-surface mode) nil)))
  (if render-surface
      (setf (state surface) t)
      (setf (state surface) :folded))
  (setf (tabs (decoration surface)) tabs)
  (when render-surface
    (resize surface (- right left) (- bottom top) (get-milliseconds)
            :activate? (eq (surface surface) (active-surface (view mode)))
            :maximize t)))

(defmethod size-changed ((mode wmii-mode) width height)
  (setf (width (layout mode)) width)
  (setf (height (layout mode)) height)
  (ulubis.wmii:reposition-all-windows (layout mode)))

(defmethod (setf title) :after (new-title (toplevel zxdg-toplevel-v6))
  (dolist (mode (append (mapcan #'modes (views *compositor*))
                        (mapcar #'default-mode (views *compositor*))))
    (when (typep mode 'wmii-mode)
      (dolist (surface (surfaces mode))
        (when (eq (surface surface) toplevel)
          (redraw-decoration (decoration surface)))))))

(defmethod first-configure ((mode wmii-mode) (surface zxdg-toplevel-v6))
  (let ((decorated (make-instance 'decorated-surface
                                  :decoration (make-instance 'wmii-decoration :mode mode)
                                  :surface surface)))
    (push decorated (surfaces mode))
    (ulubis.wmii:add-surface decorated (layout mode))))

(defmethod commit ((mode wmii-mode) surface)
  (update-decorated-position (find-decorated mode surface)))

(defmethod mouse-button-handler ((mode wmii-mode) time button state)
  ;; Activate and raise surface
  (when (and (= button #x110) (= state 1) (= 0 (mods-depressed (mods *compositor*))))
    (let ((decorated (decorated-surface-under-pointer mode
                                                    (pointer-x *compositor*)
                                                    (pointer-y *compositor*))))
      (when decorated
        (activate-surface (surface decorated) mode)
        (ulubis.wmii:set-active-surface decorated (layout mode))
	(request-render))))
  (call-next-method))

(defun decorated-surface-under-pointer (mode x y)
  (dolist (surface (surfaces mode))
    ;; Since tiled surfaces never intersect it's enough to find the first one without checking order
    (when (and (<= (x surface) x (+ (x surface) (width surface)))
               (<= (y surface) y (+ (y surface) (width surface))))
      (return-from decorated-surface-under-pointer surface))))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (let ((*ortho* (make-ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)))
    (apply #'gl:clear-color (clear-color mode))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (let* ((managed-surfaces (ulubis.wmii:surfaces (layout mode)))
             (floating-surfaces (set-difference (surfaces mode) managed-surfaces)))
        ;; Managed surfaces are drawn in no particular order
        (mapcar (lambda (surface)
                  (render surface view-fbo))
                managed-surfaces)
        (mapcar (lambda (surface)
                  (render surface view-fbo))
                (reverse floating-surfaces))))))

;;; Decoration

(defclass wmii-decoration (decoration cairo-surface)
  ((tabs :accessor tabs :initarg :tabs :initform nil :documentation "A list of toplevels tabbed with the main one")))

(defmethod initialize-instance :after ((instance wmii-decoration) &key mode)
  (setf (draw-func instance)
        (lambda ()
          (wmii-decoration-draw instance mode))))

(defun wmii-decoration-draw (decoration mode)
  (with-slots (surface) decoration
    (let ((active-surface? (eq (active-surface (view mode)) surface))
          (title (title surface)))
      (if active-surface?
          (cl-cairo2:set-source-rgba 0.3 0.2 0.0 1)
          (cl-cairo2:set-source-rgba 0.6 0.4 0.0 1))
      (cl-cairo2:paint)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1)
      (cl-cairo2:move-to 10 10)
      (cl-cairo2:show-text title))))
    

(defmethod decoration-padding ((instance wmii-decoration))
  "Top, bottom, left, right"
  (values 15 1 1 1))

(defmethod resize-decoration ((decoration wmii-decoration) width height)
  (cairo-surface-resize decoration width height)
  (call-next-method))

(defmethod redraw-decoration ((decoration wmii-decoration))
  (cairo-surface-redraw decoration))


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
                       :callback (lambda (surface top bottom left right &rest keys)
                                   (apply #'wmii-reposition-window
                                          mode surface top bottom left right keys)))))

(defmethod remove-surface-from-mode ((mode wmii-mode) surface)
  (with-slots (surfaces) mode
    (let ((decorated (find surface surfaces :key #'surface)))
      (ulubis.wmii:remove-window decorated (layout mode))
      (setf surfaces (remove decorated surfaces)))))

(defmethod allow-move? ((mode wmii-mode) surface)
  "Allow moving of unmanaged windows"
  (not (member surface (surfaces mode))))

(defun wmii-reposition-window (mode surface top bottom left right
                               &key (render-surface t) fullscreen tabbed-with)
  (setf (x surface) left)
  (setf (y surface) top)
  (cond ((fullscreen)
         (setf (state surface) :borderless)
         (setf (fullscreen-surface mode) surface))
        (t
         (setf (state surface) t)
         (setf (fullscreen-surface mode) nil)))
  (if render-surface
      (setf (state surface) t)
      (setf (state surface) :folded))
  (setf (tabbed (decoration surface)) tabbed-with)
  (resize-surface-absolute surface (view mode) (- right left) (- bottom top)))

(defmethod size-changed ((mode wmii-mode) width height)
  (setf (width (layout mode)) width)
  (setf (height (layout mode)) height))

(defmethod (setf title) :after (new-title (toplevel zxdg-toplevel-v6))
  (dolist (mode (mapcan #'modes (views *compositor*)))
    (when (typep mode 'wmii-mode)
      (dolist (surface (surfaces mode))
        (when (eq (surface surface) toplevel)
          (setf (title (decoration surface)) new-title)
          (redraw-decoration (decoration surface)))))))

(defmethod first-commit ((mode wmii-mode) (surface isurface))
  (setf (origin-x surface) 0)
  (setf (origin-y surface) 0)
  (setf (first-commit-animation surface) nil)
  (let ((decorated (make-instance 'decorated-surface
                                  :decoration (make-instance 'wmii-decoration
                                                             :width (width surface)
                                                             :height (height surface))
                                  :surface surface)))
    (push decorated (surfaces mode))
    (ulubis.wmii:add-window decorated (layout mode))))

(defmethod mouse-button-handler ((mode desktop-mode) time button state)
  ;; Activate and raise surface
  (when (and (= button #x110) (= state 1) (= 0 (mods-depressed (mods *compositor*))))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      (activate-surface surface mode)
      (when surface
	(raise-surface surface (view mode))
	(setf (render-needed *compositor*) t))))
  (call-next-method))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (let ((*ortho* (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)))
    (apply #'gl:clear-color (clear-color mode))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (let* ((managed-surfaces (ulubis.wmii:surfaces (layout mode)))
             (floating-surfaces (set-difference (surfaces (view mode)) managed-surfaces)))
        ;; Managed surfaces are drawn in no particular order
        (mapcar (lambda (surface)
                  (render surface view-fbo))
                managed-surfaces)
        (mapcar (lambda (surface)
                  (render surface view-fbo))
                (reverse floating-surfaces))))))

;;; Decoration

(defclass wmii-decoration (decoration cairo-surface)
  ((title :accessor title :initarg :title :initform "")
   (tabbed :accessor tabbed :initarg :tabbed :initform nil :documentation "A list of titles for all windows tabbed with the main one. The active tab is the one which has the same title as TITLE.")))

(defmethod initialize-instance :after ((instance wmii-decoration) &key)
  (setf (header instance) 0)
  (setf (draw-func instance)
        (lambda ()
          (cl-cairo2:set-source-rgba 1 1 1 0)
          (cl-cairo2:paint))))

(defmethod resize-decoration ((decoration wmii-decoration) width height)
  (cairo-surface-resize decoration width height)
  (call-next-method))

(defmethod redraw-decoration ((decoration wmii-decoration))
  (cairo-surface-redraw decoration))

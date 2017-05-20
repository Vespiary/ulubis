
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
                       :width 640
                       :height 480
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
  (not (surface-managed-p mode surface)))

(defmethod ulubis.panels:render-panels ((mode wmii-mode))
  (not (fullscreen-surface mode)))

(defun wmii-reposition-window (mode surface top bottom left right
                               &key (render-surface t) (visible t) fullscreen tabs)
  (unless visible
    (setf (state surface) nil)
    (return-from wmii-reposition-window))
  (cond (fullscreen
         (setf (state surface) :borderless)
         (setf (fullscreen-surface mode) surface))
        ((not render-surface)
         (setf (state surface) :folded))
        (t
         (setf (state surface) t)
         (setf (fullscreen-surface mode) nil)))
  (setf (tabs (decoration surface)) tabs)
  (resize surface (- right left) (- bottom top) (get-milliseconds)
          :activate? (eq (surface surface) (active-surface (view mode)))
          :fullscreen t)
  (when (first-commit? (wl-surface (surface surface)))
    (setf (state surface) nil))
  (setf (x surface) left)
  (setf (y surface) top))

(defmethod active-surface ((mode wmii-mode))
  (find (active-surface (view mode)) (surfaces mode) :key #'surface))

(defmethod size-changed ((mode wmii-mode) width height)
  (setf (width (layout mode)) (desktop-width))
  (setf (height (layout mode)) (desktop-height))
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
                                  :state nil
                                  :surface surface)))
    (push decorated (surfaces mode))
    (ulubis.wmii:add-surface decorated (layout mode))))

(defmethod first-commit ((mode wmii-mode) surface)
  (let ((decorated (find surface (surfaces mode) :key #'surface)))
    (setf (state decorated) t)
    (activate-surface surface mode)))

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
  ;; Drag window
  (when (and (= button #x110) (= state 1) (= Gui (mods-depressed (mods *compositor*))))
    (let ((surface (decorated-surface-under-pointer mode (pointer-x *compositor*) (pointer-y *compositor*))))
      (when surface
	(setf (moving-surface *compositor*)
	      (make-move-op :surface surface
			    :surface-x (x surface)
			    :surface-y (y surface)
			    :pointer-x (pointer-x *compositor*)
			    :pointer-y (pointer-y *compositor*))))))
  ;; Resize window
  (when (and (= button #x110) (= state 1) (= (+ Gui Shift) (mods-depressed (mods *compositor*))))
    (let ((surface (decorated-surface-under-pointer mode (pointer-x *compositor*) (pointer-y *compositor*))))
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
  ;; Stop resizing
  (when (and (resizing-surface *compositor*) (= button #x110) (= state 0))
    (setf (resizing-surface *compositor*) nil))
  ;; Send button to client
  (call-next-method))

(defun decorated-surface-under-pointer (mode x y)
  (dolist (surface (floating-surfaces mode))
    (when (and (<= (x surface) x (+ (x surface) (width surface)))
               (<= (y surface) y (+ (y surface) (height surface))))
      (return-from decorated-surface-under-pointer surface)))
  (dolist (surface (ulubis.wmii:surfaces (layout mode)))
    ;; Since tiled surfaces never intersect it's enough to find the first one without checking order
    (when (and (<= (x surface) x (+ (x surface) (width surface)))
               (<= (y surface) y (+ (y surface) (height surface))))
      (return-from decorated-surface-under-pointer surface)))
  nil)

(defun surface-managed-p (mode surface)
  (find surface (ulubis.wmii:surfaces (layout mode))))

(defun floating-surfaces (mode)
  (set-difference (surfaces mode) (ulubis.wmii:surfaces (layout mode))))

(defmethod render ((mode wmii-mode) &optional view-fbo)
  (let ((*ortho* (make-ortho 0 (desktop-width) (desktop-height) 0 1 -1)))
    (apply #'gl:clear-color (clear-color mode))
    (when view-fbo
      (cepl:clear view-fbo))
    (cepl:with-blending (blending-parameters mode)
      (if (fullscreen-surface mode)
          (render (fullscreen-surface mode) view-fbo)
          (let* ((managed-surfaces (ulubis.wmii:surfaces (layout mode)))
                 (floating-surfaces (set-difference (surfaces mode) managed-surfaces)))
            ;; Managed surfaces are drawn in no particular order
            (mapcar (lambda (surface)
                      (render surface view-fbo))
                    managed-surfaces)
            (mapcar (lambda (surface)
                      (render surface view-fbo))
                    (reverse floating-surfaces)))))))

;;; Decorations

(defclass wmii-decoration (decoration ulubis.cairo:surface)
  ((tabs :accessor tabs :initarg :tabs :initform nil :documentation "A list of toplevels tabbed with the main one")))

(defmethod initialize-instance :after ((instance wmii-decoration) &key mode)
  (setf (ulubis.cairo:draw-func instance)
        (lambda ()
          (wmii-decoration-draw instance mode))))

(defun wmii-decoration-draw (decoration mode)
  (flet ((active-color ()
           (cl-cairo2:set-source-rgb 0.506 0.396 0.310))
         (inactive-color ()
           (cl-cairo2:set-source-rgb 0.659 0.616 0.588))
         (black-color ()
           (cl-cairo2:set-source-rgb 0 0 0)))
    (with-slots (surface tabs) decoration
      (let ((width (width decoration))
            (height (height decoration))
            (active-surface (eq (active-surface (view mode)) surface))
            (title (title surface)))
        (cl-cairo2:set-line-width 1)
        (if active-surface
            (active-color)
            (inactive-color))
        (cl-cairo2:paint)
        ;; Window border
        (draw-outset 0.5 0.5 (- width 1) (- height 1))
        ;; Header
        (if tabs
            (let ((tab-width (truncate width (length tabs))))
              (loop :for tab :in tabs
                 :for i :from 0
                 :do (wmii-decoration-draw-tab (* i tab-width) 0
                                               tab-width 15
                                               :title (title (surface tab))
                                               :active-tab nil
                                               :active-surface active-surface))
              (wmii-decoration-draw-tab (* (position decoration tabs :key #'decoration) tab-width) 0
                                        tab-width 15
                                        :title title
                                        :active-tab t
                                        :active-surface active-surface))
            (wmii-decoration-draw-tab 0 0 width 15
                                      :title title
                                      :active-tab t
                                      :active-surface active-surface))
        ))))

(defun wmii-decoration-draw-tab (x y width height &key title active-tab active-surface)
  (flet ((active-color ()
           (cl-cairo2:set-source-rgb 0.506 0.396 0.310))
         (inactive-color ()
           (cl-cairo2:set-source-rgb 0.659 0.616 0.588))
         (black-color ()
           (cl-cairo2:set-source-rgb 0 0 0)))
    (cl-cairo2:rectangle x y width height)
    (cl-cairo2:clip)
    (cl-cairo2:translate x y)
    ;; Background
    (if (and active-tab active-surface)
        (active-color)
        (inactive-color))
    (cl-cairo2:paint)
    ;; Title
    (black-color)
    (cl-cairo2:move-to 17 11)
    (cl-cairo2:show-text title)
    ;; Header border
    (draw-outset 0.5 0.5 (- width 1) (- height 1) :bottom (not active-tab))
    ;; Square
    (black-color)
    (cl-cairo2:rectangle 2.5 2.5 11 11)
    (cl-cairo2:stroke)
    (cl-cairo2:translate (- x) (- y))
    (cl-cairo2:reset-clip)))

(defun draw-outset (x y width height &key
                                       (highlight '(1 1 1))
                                       (shadow '(0.506 0.396 0.310))
                                       (bottom t))
  (cl-cairo2:move-to x (+ y height))
  (cl-cairo2:line-to x y)
  (cl-cairo2:line-to (+ x width) y)
  (apply #'cl-cairo2:set-source-rgb highlight)
  (cl-cairo2:stroke)
  (cl-cairo2:move-to (+ x width) y)
  (cl-cairo2:line-to (+ x width) (+ y height))
  (when bottom
    (cl-cairo2:line-to x (+ y height)))
  (apply #'cl-cairo2:set-source-rgb shadow)
  (cl-cairo2:stroke))
    

(defmethod decoration-padding ((instance wmii-decoration))
  "Top, bottom, left, right"
  (values 16 2 2 2))

(defmethod resize-decoration ((decoration wmii-decoration) width height)
  (ulubis.cairo:resize decoration width height)
  (call-next-method))

(defmethod redraw-decoration ((decoration wmii-decoration))
  (ulubis.cairo:redraw decoration))

;; Keybindings

(defkeybinding (:pressed "h" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-window-left (layout mode))))

(defkeybinding (:pressed "j" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-window-down (layout mode))))

(defkeybinding (:pressed "k" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-window-up (layout mode))))

(defkeybinding (:pressed "l" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-window-right (layout mode))))

(defkeybinding (:pressed "h" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-cursor-left (layout mode))))

(defkeybinding (:pressed "j" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-cursor-down (layout mode))))

(defkeybinding (:pressed "k" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-cursor-up (layout mode))))

(defkeybinding (:pressed "l" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:move-cursor-right (layout mode))))

(defkeybinding (:pressed "d" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:set-column-mode (layout mode) :even)))

(defkeybinding (:pressed "s" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:set-column-mode (layout mode) :stack)))

(defkeybinding (:pressed "a" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:set-column-mode (layout mode) :tabs)))

(defkeybinding (:pressed "u" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:resize-column (layout mode) :left 0.1)))

(defkeybinding (:pressed "i" Ctrl ) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:resize-column (layout mode) :right 0.1)))

(defkeybinding (:pressed "u" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:resize-column (layout mode) :right -0.1)))

(defkeybinding (:pressed "i" Ctrl Shift) (mode) (wmii-mode)
  (unless (fullscreen-surface mode)
    (ulubis.wmii:resize-column (layout mode) :left -0.1)))

(defkeybinding (:pressed "f" Ctrl Shift) (mode) (wmii-mode)
  (ulubis.wmii:toggle-fullscreen (layout mode)))

(defkeybinding (:pressed "c" Ctrl Shift) (mode) (wmii-mode)
  (let ((active-surface (active-surface (view mode))))
    (when active-surface
      (kill-client (client active-surface)))))

(defkeybinding (:pressed "Space" Ctrl Shift) (mode) (wmii-mode)
  (let ((active-surface (active-surface mode)))
    (if (surface-managed-p mode active-surface)
        (ulubis.wmii:remove-surface active-surface (layout mode))
        (ulubis.wmii:add-surface active-surface (layout mode)))))

;; Here we define a mode system
;; Modes encapsulate a behaviour
;; I.e. we can set a custom rendering function,
;; custom shortcuts, etc.

(in-package :ulubis)

(defclass mode (ulubis.panels:panel-container)
  ((blending-parameters :accessor blending-parameters :initarg :blending-parameters :initform nil)
   (focus-follows-mouse :accessor focus-follows-mouse :initarg :focus-follows-mouse :initform nil)))

;; We introduce defmode. Ideally we would just subclass mode,
;; but we'd like to a class allocated slot KEY-BINDINGS. We
;; can't inherit this from MODE because the subclasses would
;; share that allocation. Instead we introduce this macro
;; to include a subclass-specific KEY-BINDINGS.
(defmacro defmode (name (&rest superclasses) (&body slots))
  "DEFMODE automatically inherits MODE and provides class-allocated slot KEY-BINDINGS"
  `(defclass ,name (,@superclasses mode)
     ((key-bindings :accessor key-bindings :initarg :key-bindings :initform nil :allocation :class)
      (view :accessor view :initarg :view :initform nil)
      ,@slots)))

(defgeneric init-mode (mode))
(defgeneric mouse-motion-handler (mode time delta-x delta-y))
(defgeneric mouse-button-handler (mode time button state))
(defgeneric keyboard-handler (mode time keycode state))
(defgeneric render (mode &optional view-fbo))
(defgeneric first-configure (mode surface))
(defgeneric first-commit (mode surface))
(defgeneric commit (mode surface))
(defgeneric size-changed (mode width height))
(defgeneric remove-surface-from-mode (mode surface))
(defgeneric allow-move? (mode surface))

(defmethod init-mode :before ((mode mode))
  (setf (blending-parameters mode) (cepl:make-blending-params)))

(defmethod init-mode ((mode mode))
  (cepl:map-g #'mapping-pipeline nil)
  (request-render))

(defmethod first-configure ((mode mode) toplevel)
  (with-wl-array array
    (zxdg-toplevel-v6-send-configure (->resource toplevel) 0 0 array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 toplevel)) 0)))

(defmethod first-commit ((mode mode) surface)
  )

(defmethod commit ((mode mode) surface)
  )

(defmethod size-changed ((mode mode) width height)
  )

(defmethod allow-move? ((mode mode) surface)
  t)

(defmethod remove-surface-from-mode ((mode mode) surface)
  )

(defmethod ulubis.panels:delegate ((mode mode))
  (view mode))

(defun push-mode (view mode)
  (setf (view mode) view)
  (init-mode mode)
  (push mode (modes view))
  (request-render))
  
(defun pop-mode (mode)
  (with-slots (view) mode
    (pop (modes view))))

;;; Default keyboard handling

(defclass key-binding ()
  ((op :accessor op :initarg :op :initform :pressed)
   (key :accessor key :initarg :key :initform nil)
   (mods :accessor mods :initarg :mods :initform nil)
   (fn :accessor fn :initarg :fn :initform (lambda ()))))

(defconstant Shift 1)
(defconstant Ctrl 4)
(defconstant Alt 8)
(defconstant Gui 64)

;; We create a dummy instance of each mode. Not pretty
;; but alternatively we can use mop:class-prototype
;; to get access to a non-consed class-allocated slot
(defun register-keybinding (op rawkey mods modes fn)
  (let ((key (etypecase rawkey
	       (string (xkb:get-keysym-from-name rawkey :case-insensitive t))
	       (number rawkey)
	       (null nil))))
    (if (eq key 0)
	(format t "Unknown key ~A~%" rawkey)
	(loop :for mode :in modes :do
	   (let ((instance (make-instance mode))
		 (new-kb (make-instance 'key-binding
					:op op
					:key key
					:mods (apply #'logior mods)
					:fn fn))
		 (test (lambda (new old)
			 (and (eq (op new) (op old))
			      (eq (key new) (key old))
			      (eq (mods new) (mods old))))))
	     (setf (key-bindings instance) (delete new-kb (key-bindings instance) :test test))
	     (push new-kb (key-bindings instance)))))))

(defmacro defkeybinding ((op rawkey &rest mods) (&optional mode-ref) modes &body body)
  `(register-keybinding ,op ,rawkey (list ,@mods) ',modes
                        ,(if mode-ref
                             `(lambda (,mode-ref)
                                ,@body)
                             ;; Keyboard handler will pass the mode anyway
                             (let ((dummy-var (gensym "DUMMY")))
                               `(lambda (,dummy-var)
                                  (declare (ignore ,dummy-var))
                                  ,@body)))))

(defun cancel-mods (surface)
  (when (and surface (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
				0
				0
				0
				0)))

(defun keyboard-keybinds-handler (mode keysym state)
  "Must return T if the hotkey is handled"
  (let ((surface (active-surface (view mode)))
        (keysym (xkb:tolower keysym)))
    (loop :for key-binding :in (key-bindings mode) :do
       (with-slots (op key mods fn) key-binding
         (cond 
           ((and (eq op :pressed)
                  (or (not keysym) (= keysym key))
                  (= 1 state)
                  (or (zerop mods) (= (mods-depressed (mods *compositor*)) mods)))
            (format t "Calling pressed~%")
            (cancel-mods surface)
            (funcall fn mode)
            (return-from keyboard-keybinds-handler t))
           ((and (eq op :released)
                 (= 0 state)
                 (or (not key) (and keysym (= keysym key) (= state 0)))
                 (zerop (logand (mods-depressed (mods *compositor*)) mods)))
            (format t "Calling released~%")
            (cancel-mods surface)
            (funcall fn mode)
            (return-from keyboard-keybinds-handler t)))))
    nil))

(defmethod keyboard-handler (view time keycode state)
  (let ((surface (active-surface view)))
    (when (and surface keycode (keyboard (client surface)))
      (wl-keyboard-send-key (->resource (keyboard (client surface))) 0 time keycode state))
    (when (and surface (keyboard (client surface)))
      (apply #'wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
             (mods *compositor*)))))


;;; Default mouse handling

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
  (unless (typep mode 'wmii-mode)
    (when (and new-surface (pointer (client new-surface)))
      (wl-pointer-send-enter (->resource (pointer (client new-surface)))
                             0
                             (->resource (wl-surface new-surface))
                             (round (* 256 (- x (x new-surface))))
                             (round (* 256 (- y (y new-surface))))))))

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

(defmethod mouse-motion-handler ((mode mode) time delta-x delta-y)
  ;; Update the pointer location
  (with-slots (pointer-x pointer-y) *compositor*
    (update-pointer delta-x delta-y)
    (when (cursor-surface *compositor*)
      (request-render))
    (let ((old-surface (pointer-surface *compositor*))
	  (current-surface (surface-under-pointer pointer-x pointer-y (view mode))))
      (cond
	;; 1. If we are dragging a window...
	((moving-surface *compositor*)
	 (move-surface pointer-x pointer-y (moving-surface *compositor*)))
	;; 2. If we are resizing a window...
	((resizing-surface *compositor*)
	 (resize-surface pointer-x pointer-y (view mode) (resizing-surface *compositor*)))
	;; 3. The pointer has left the current surface
        ((not (equalp old-surface current-surface))
	 (setf (cursor-surface *compositor*) nil)
	 (pointer-changed-surface mode pointer-x pointer-y old-surface current-surface))
	;; 4. Pointer is over previous surface
	((equalp old-surface current-surface)
	 (send-surface-pointer-motion pointer-x pointer-y time current-surface))))))

(defmethod mouse-button-handler ((mode mode) time button state)
  ;; Send active surface mouse button
  (let ((surface (surface-under-pointer (pointer-x *compositor*)
                                        (pointer-y *compositor*)
                                        (view mode))))
    (when (and surface (pointer (client surface)))
      (wl-pointer-send-button (->resource (pointer (client surface)))
                              0
                              time
                              button
                              state)))
  ;; stop drag (It can be started by the client)
  (when (and (moving-surface *compositor*) (= button #x110) (= state 0))
    (setf (moving-surface *compositor*) nil)))

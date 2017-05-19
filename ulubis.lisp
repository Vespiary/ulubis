
(in-package :ulubis)

(declaim (optimize (debug 3)))

(defparameter *enable-debugger* nil)
(defparameter *compositor* nil)

(defun draw-screen ()
  (cepl:clear)
  (let ((top-panel (find-panel :top))
        (bottom-panel (find-panel :bottom)))
    (when top-panel
      (ulubis.cairo:redraw top-panel)
      (map-to-viewport (texture-of top-panel)
                       0 0
                       (desktop-width)
                       (ulubis.panels:height top-panel)
                       #'panel-pipeline))
    (map-to-viewport (texture-of (current-view *compositor*))
                     0 (ulubis.panels:height top-panel)
                     (desktop-width) (desktop-height))
    (when bottom-panel
      (ulubis.cairo:redraw bottom-panel)
      (map-to-viewport (texture-of bottom-panel)
                       0 (+ (ulubis.panels:height top-panel) (desktop-height))
                       (desktop-width) (ulubis.panels:height bottom-panel)
                       #'panel-pipeline)))
  (gl:enable :blend)
  (draw-cursor nil nil (pointer-x *compositor*) (pointer-y *compositor*)
               (make-ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1))
  #+nil(draw-cursor (cursor-surface *compositor*) nil (pointer-x *compositor*) (pointer-y *compositor*)
               (make-ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1))
  (swap-buffers (backend *compositor*))
  (setf (slot-value *compositor* 'render-needed) nil)
  (finish-callbacks))

(defun finish-callbacks ()
  (loop :for callback :in (callbacks *compositor*) :do
     (when (find (client callback) waylisp::*clients*)
       ;; We can end up getting a frame request after the client has been deleted
       ;; if we try and send-done or destroy we will get a memory fault
       (wl-callback-send-done (->resource callback) (get-milliseconds))
       (wl-resource-destroy (->resource callback)))
     (remove-resource callback))
  (setf (callbacks *compositor*) nil))

(defun map-to-viewport (texture x y width height &optional (shader #'passthrough-shader))
  (cepl:with-viewport (cepl:make-viewport (list width height)
                                          (list x (- (screen-height *compositor*) height y)))
    (with-screen (vs)
      (cepl:map-g shader vs :texture texture))))

(defcallback input-callback :void ((fd :int) (mask :int) (data :pointer))
  (process-events (backend *compositor*)))

(defun main-loop-drm (event-loop)
  (let ((libinput-fd (get-fd (backend *compositor*))))
    (initialize-animation event-loop)
    (wl-event-loop-add-fd event-loop libinput-fd 1 (callback input-callback) (null-pointer))
    (event-loop-add-drm-fd (backend *compositor*) event-loop)
    (loop :while (running *compositor*)
       :do (with-simple-restart (skip-event "Skip handling this event")
	     (when (and (slot-value *compositor* 'render-needed) (not (get-scheduled (backend *compositor*))))
	       (draw-screen))
	     (wl-display-flush-clients (display *compositor*))
	     (wl-event-loop-dispatch event-loop -1)
             (mapc #'ulubis.panels:update ulubis.panels::*status-bars*)
	     (animation::update-animations #'request-render)))))

(defun main-loop-sdl (event-loop)
  (let ((wayland-fd (wl-event-loop-get-fd event-loop)))
    (syscall:with-pollfds (pollfds
			   (wayland-pollfd wayland-fd syscall:pollin syscall:pollpri))
      (initialize-animation event-loop)
      (loop :while (running *compositor*)
	 :do (with-simple-restart (skip-event "Skip handling this event")
	       (when (slot-value *compositor* 'render-needed)
		 (draw-screen))
	       (wl-event-loop-dispatch event-loop 0)
	       (wl-display-flush-clients (display *compositor*))
	       (alexandria:ignore-some-conditions (nix:eintr)
		 (let ((event (syscall:poll pollfds 1 5)))
		   (wl-event-loop-dispatch event-loop 0)
		   (wl-display-flush-clients (display *compositor*))
                   (mapc #'ulubis.panels:update ulubis.panels::*status-bars*)
		   (animation::update-animations #'request-render)
		   (process-events (backend *compositor*)))))))))

(defun resize-surface-relative (surface view delta-x delta-y)
  (when (and (or (ulubis-zxdg-toplevel? surface) (ulubis-xdg-surface? surface)) (> (+ delta-x width) 32) (> (+ delta-y height) 32))
    (if (equalp surface (active-surface view))
	(waylisp:resize surface width height (get-milliseconds) :activate? t)
	(waylisp:resize surface width height (get-milliseconds) :activate? nil))))

(defun resize-surface-absolute (surface view width height)
  (when (and (> width 32) (> height 32))
    (let ((activate (eq surface (active-surface view))))
      (resize surface width height (get-milliseconds)
              :activate? activate
              :maximize (requested-max (surface surface))
              :fullscreen (requested-full (surface surface))))))

#|
(defun deactivate-surface (surface)
  (when surface
    (waylisp:keyboard-send-leave surface)
    (when (or (ulubis-xdg-surface? surface) (ulubis-zxdg-toplevel? surface))
      (waylisp:deactivate surface (get-milliseconds)))))

(defun activate-surface (surface view)
  (with-slots (active-surface) view
    (cond
      ;; No surface to activate
      ((not surface) (progn
		       (deactivate-surface active-surface)
		       (setf active-surface nil)))
      ;; Activating a non-active surface
      ((and surface (not (waylisp:wl-cursor? surface)) (not (equalp surface active-surface)))
       (progn
	 (deactivate-surface active-surface)
	 (setf active-surface surface)
	 (keyboard-send-enter surface)
	 (keyboard-send-modifiers surface
				  (mods-depressed *compositor*)
				  (mods-latched *compositor*)
				  (mods-locked *compositor*)
				  (mods-group *compositor*))
	 (when (or (ulubis-xdg-surface? surface) (ulubis-zxdg-toplevel? surface))
	   (waylisp:activate surface (get-milliseconds))))))))
|#

(defun kill-client (client)
  (nix:kill (wayland-server-core:pid-of-client (->client client)) nix:sigkill))

(defun activate-surface (surface mode)
  (with-slots (active-surface) (view mode)
    (setf active-surface
          (activate surface active-surface (mods *compositor*)))))

(defun call-mouse-motion-handler (time x y)
  (when (show-cursor *compositor*)
    (request-render))
  (mouse-motion-handler (current-mode *compositor*) time x y))

;; Should be able to have "active" window without raising (focus follows mouse)
(defun call-mouse-button-handler (time button state)
  (mouse-button-handler (current-mode *compositor*) time button state))

(defun window-event-handler ()
  (resize-compositor)
  (request-render))

(defun call-keyboard-handler (time keycode state)
  (with-slots (xkb-input xkb-keybinds) *compositor*
    (ulubis.xkb:update-key xkb-input keycode state)
    (ulubis.xkb:update-key xkb-keybinds keycode state)
    (let ((keysym (ulubis.xkb:last-pressed-key xkb-keybinds)))
      (setf (mods *compositor*) (ulubis.xkb:serialize-mods (xkb-input *compositor*)))
      (when (= state 1)
        (format t "Keycode: ~A; Keysym: ~A; Mods: ~A~%"
                keycode keysym (mods-depressed (mods *compositor*))))
      (when (and (numberp keysym) (numberp state))
        ;; Send to the active client unless handled by Ulubis
        (unless (keyboard-keybinds-handler (current-mode *compositor*)
                                          keysym
                                          state)
          (keyboard-handler (current-view *compositor*)
                            time
                            keycode
                            state))))))

(defun resize-compositor ()
  (let ((size (cepl.host:window-size (cepl.context::window cepl.context:*gl-context*))))
    (unless (and (equal size (cepl.viewports:viewport-dimensions (cepl.viewports:current-viewport)))
                 (equal size (list (screen-width *compositor*)
                                   (screen-height *compositor*))))
      (format t "Using new screen size: ~A~%" size)
      (setf (cepl.viewports:viewport-dimensions (cepl.viewports:current-viewport))
            size)
      (setf (screen-width *compositor*) (first size)
            (screen-height *compositor*) (second size))
      (update-ortho)
      (mapc #'view-ensure-valid-fbo (views *compositor*))
      (dolist (mode (append (mapcan #'modes (views *compositor*))
                            (mapcar #'default-mode (views *compositor*))))
        (size-changed mode (first size) (second size)))
      (let ((top (find-panel :top))
            (bottom (find-panel :bottom)))
        (when top
          (ulubis.cairo:resize top (desktop-width) 15))
        (when bottom
          (ulubis.cairo:resize bottom (desktop-width) 15))))))

(defun try-load-user-init-file (filename)
  (format t "Trying to load ~A~%" filename)
  (when (probe-file filename)
    (format t "Loading ~A~%" filename)
    (let ((*package* (find-package :ulubis.config)))
      (load filename))))

(defun perform-user-init ()
  ;; Load user configuration file
  ;; Look for ulubis/init.lisp in xdg configuration dirs (home first)
  ;; Failing that try to load ~/.ulubis.lisp
  (unless (some #'try-load-user-init-file
                `(,(uiop:xdg-config-home "ulubis/init.lisp")
                  ,@(uiop:xdg-config-dirs "ulubis/init.lisp")
                  ,(uiop:merge-pathnames* ".ulubis.lisp" (user-homedir-pathname))))
    (format t "No configuration file found!~%"))

  ;; Ensure that there's at least one view
  (when (zerop (length (views *compositor*)))
    (push-view 'desktop-mode))

  ;; Ensure that there's a current view
  (unless (current-view *compositor*)
    (setf (current-view *compositor*) (first (views *compositor*)))))

(defun initialise ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  
  ;; Make our compositor class
  (setf *compositor* (make-instance 'compositor))
  
  ;; Initialise backend
  (setf (backend *compositor*) (make-instance 'backend))
  (initialise-backend (backend *compositor*)
                      640
                      480
                      (devices *compositor*))
  (resize-compositor)

  (perform-user-init)

  (register-mouse-motion-handler (backend *compositor*) 'call-mouse-motion-handler)
  (register-mouse-button-handler (backend *compositor*) 'call-mouse-button-handler)
  (register-window-event-handler (backend *compositor*) 'window-event-handler)
  (register-keyboard-handler (backend *compositor*) 'call-keyboard-handler)
  
  ;; Create our wayland display
  (setf (display *compositor*) (wl-display-create))
  (format t "Opened socket: ~A~%" (wl-display-add-socket-auto (display *compositor*)))

  ;; Initialise shared memory

  
  (initialize-wayland-server-interfaces) 
  (initialize-xdg-shell-server-interfaces)
  (initialize-zxdg-shell-v6-server-interfaces) 
  ;;(set-implementations)
  (set-implementation-wl-surface)
  (set-implementation-wl-seat)
  (set-implementation-wl-pointer)
  (set-implementation-wl-seat)
  ;;(set-implementation-wl-callback)
  (set-implementation-wl-region)
  (set-implementation-wl-compositor)
  (set-implementation-wl-subcompositor)
  (set-implementation-wl-subsurface)
  (set-implementation-wl-output)	 
  (set-implementation-wl-shell)
  (set-implementation-wl-shell-surface)
  (set-implementation-wl-data-device-manager)
  (set-implementation-wl-data-device)
  (set-implementation-wl-data-source)
  (set-implementation-zxdg-shell-v6)
  (set-implementation-zxdg-surface-v6)
  (set-implementation-zxdg-toplevel-v6)
  (set-implementation-xdg-shell)
  (set-implementation-xdg-surface)

  (wl-display-init-shm (display *compositor*))

  (wl-global-create (display *compositor*) 
                    wl-output-interface
                    2
                    (null-pointer)
                    (callback output-bind))

  (wl-global-create (display *compositor*)
                    wl-compositor-interface
                    3
                    (null-pointer)
                    (callback compositor-bind))
  
  (wl-global-create (display *compositor*)
                    wl-shell-interface
                    1
                    (null-pointer)
                    (callback shell-bind))

  (wl-global-create (display *compositor*)
                    wl-seat-interface
                    3
                    (null-pointer)
                    (callback seat-bind))

  (wl-global-create (display *compositor*)
                    wl-data-device-manager-interface
                    3
                    (null-pointer)
                    (callback device-manager-bind))

  (wl-global-create (display *compositor*) 
                    wl-subcompositor-interface
                    1
                    (null-pointer)
                    (callback subcompositor-bind))
  
  (wl-global-create (display *compositor*)
                    zxdg-shell-v6-interface
                    1
                    (null-pointer)
                    (callback zxdg-shell-v6-bind))
  
  (wl-global-create (display *compositor*)
                    xdg-shell-interface
                    1
                    (null-pointer)
                    (callback xdg-shell-bind)))

(defun run-main-loop ()
  (setf (running *compositor*) t)
  (if (string-equal (symbol-name backend-name) "backend-drm-gbm")
      (main-loop-drm (wl-display-get-event-loop (display *compositor*)))
      (main-loop-sdl (wl-display-get-event-loop (display *compositor*)))))

(defun run-compositor ()
  (unwind-protect
       (with-simple-restart (terminate "Terminate Ulubis")
         (handler-bind (#+sbcl
                        (sb-sys:interactive-interrupt
                         (lambda (e)
                           (format t "Caught SIGINT. Exiting")
                           (return-from run-compositor)))
                        (error
                         (lambda (e)
                           (when *enable-debugger*
                             (invoke-debugger e))
                           (format t "~%Oops! Something went wrong with ulubis...we throw ourselves at your mercy! Exiting wih error:~%")
                           (trivial-backtrace:print-backtrace e)
                           (return-from run-compositor))))
           (initialise)
           (run-main-loop)))
    (when (display *compositor*)
      (wl-display-destroy (display *compositor*))
      (setf (display *compositor*) nil))
    (destroy-backend (backend *compositor*))
    (setf *compositor* nil)))

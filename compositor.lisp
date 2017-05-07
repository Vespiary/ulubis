
(in-package :ulubis)

(defparameter *compositor* nil)

(defun run-program (string)
  #+sbcl
  (sb-ext:run-program string '() :wait nil)
  #+ccl
  (ccl:run-program string '() :wait nil))

(defclass compositor ()
  ((running :accessor running :initarg :running :initform t)
   (backend :accessor backend :initarg :backend :initform nil)
   (display :accessor display :initarg :display :initform nil)
   (devices :accessor devices :initarg :devices :initform nil)
   (callbacks :accessor callbacks :initarg :callbacks :initform nil)
   (->output :accessor ->output :initarg :->output :initform nil)
   (event-loop :accessor event-loop :initarg :event-loop :initform nil)
   (screen-width :accessor screen-width :initarg :screen-width :initform 640)
   (screen-height :accessor screen-height :initarg :screen-height :initform 480)
   (views :accessor views :initarg :views :initform nil) ;; e.g. virtual desktops
   (current-view :accessor current-view :initarg current-view :initform nil)
   (surfaces :accessor surfaces :initarg :surfaces :initform nil)
   (clients :accessor clients :initarg :clients :initform nil)
   (moving-surface :accessor moving-surface :initarg :moving-surface :initform nil)
   (resizing-surface :accessor resizing-surface :initarg :resizing-surface :initform nil)
   (pointer-surface :accessor pointer-surface :initarg :pointer-surface :initform nil)
   (cursor-surface :accessor cursor-surface :initarg :cursor-surface :initform nil)
   (show-cursor :accessor show-cursor :initarg :show-cursor :initform t)
   (pointer-x :accessor pointer-x :initarg :pointer-x :initform 0)
   (pointer-y :accessor pointer-y :initarg :pointer-y :initform 0)
   (data-devices :accessor data-devices :initarg :data-devices :initform nil)
   (render-needed :accessor render-needed :initarg :render-needed :initform nil)
   (xkb-input :accessor xkb-input :initarg :xkb-state :initform nil)
   (xkb-keybinds :accessor xkb-keybinds :initarg :xkb-state :initform nil)
   (mods :accessor mods :initarg :mods :initform 0)))

(defstruct (mods (:type list))
  depressed latched locked layout)

(defmethod initialize-instance :after ((compositor compositor) &key)
  (setf (xkb-keybinds compositor) (make-instance 'ulubis.xkb:state :layout "us"))
  (setf (xkb-input compositor) (make-instance 'ulubis.xkb:state :layout "us")))
	   
(defun set-keymap (compositor r m l v o)
  (with-slots (xkb-input) compositor
    (when xkb-input
      (ulubis.xkb:free xkb-input))
    (setf xkb-input
          (make-instance 'ulubis.xkb:state
                         :layout l
                         :variant v
                         :options o))))

(defun get-keymap (compositor)
  (let* ((string (ulubis.xkb:state-keymap-name (xkb-input compositor)))
	 (size (+ (length string) 1))
	 (xdg-runtime-dir (nix:getenv "XDG_RUNTIME_DIR"))
	 (fd (nix:mkstemp (concatenate 'string xdg-runtime-dir "/XXXXXXXX"))))
;;    (multiple-value-bind (fd name) (nix:mkstemp (concatenate 'string xdg-runtime-dir "/XXXXXXXX"))
    (nix:ftruncate fd size)
    (let ((map (nix:mmap (null-pointer) size (logior nix:prot-read nix:prot-write) nix:map-shared fd 0)))
      (lisp-string-to-foreign string map size)
      (nix:munmap map size)
      (values fd size))))

#|
(defun find-client (client-pointer compositor)
  (find-if (lambda (client)
	     (and (pointerp (waylisp:->client client)) (pointer-eq (waylisp:->client client) client-pointer)))
	   (clients compositor)))


(defun find-surface (surface-pointer compositor)
  (find-if (lambda (surface)
	     (and (pointerp (waylisp:->surface surface)) (pointer-eq (waylisp:->surface surface) surface-pointer)))
	   (surfaces compositor)))

(defun find-region-of-client (->client ->region compositor)
  (waylisp:find-region ->region (waylisp:get-client ->client)))

(defun find-client-with-surface (surface-pointer compositor)
  (find-if (lambda (client)
	     (find-if (lambda (surface)
			(and (pointerp (waylisp:->surface surface)) (pointer-eq (waylisp:->surface surface) surface-pointer)))
		      (surfaces client)))
	   (clients compositor)))
|#

(defun remove-client (client-pointer)
  (let ((client (get-client client-pointer)))
    (loop :for resource :in (resources client) :do
       (format t "resource: ~A~%" resource)
       (remove-surface resource *compositor*))
    (setf (resources client) nil)
    (setf waylisp::*clients* (remove-if (lambda (client)
				 (and (pointerp (waylisp:->client client)) (pointer-eq (waylisp:->client client) client-pointer)))
			       waylisp::*clients*))))

(defun view-has-surface? (surface view)
  (when (find surface (surfaces view))
    view))

(defun views-with-surface (surface)
  (loop :for view :in (views *compositor*)
     :when (view-has-surface? surface view) :collect it))

(defun remove-surface-from-view (surface view)
  (format t "surface: ~A, view: ~A~%" surface view)
  (when (equalp (active-surface view) surface)
    (setf (active-surface view) nil))
  (setf (surfaces view) (remove surface (surfaces view))))

(defun remove-surface (surface compositor)
  (let* ((views (views-with-surface surface)))
    (loop :for view :in views :do (remove-surface-from-view surface view))
    ;; TODO do we need to do the same for MOVING-SURFACE and RESIZING-SURFACE
    (when (equalp surface (pointer-surface *compositor*))
      (setf (pointer-surface *compositor*) nil))
    (setf (surfaces compositor) (remove surface (surfaces compositor)))))

(defun raise-surface (surface view)
  (when surface
    (setf (surfaces view) (cons surface (remove surface (surfaces view))))))

(defstruct move-op
  surface
  surface-x
  surface-y
  pointer-x
  pointer-y)

(defstruct resize-op
  surface
  pointer-x
  pointer-y
  surface-width
  surface-height)

;; Check pointer is over client
;; If it is and there is no input-region return true
;; It it is and there is an input-region

(defun pointer-over-p (pointer-x pointer-y x y width height)
  "Return true if pointer is within rect defined by x y width and height. pointer-x and pointer-y are local to the client surface"
  (and (>= pointer-x x) (<= pointer-x (+ x width))
       (>= pointer-y y) (<= pointer-y (+ y height))))

(defun pointer-over-input-region-p (pointer-x pointer-y surface-w/input-region)
  (let ((global-x (x surface-w/input-region))
	(global-y (y surface-w/input-region))
	(rects (rects (input-region (wl-surface surface-w/input-region)))))
    (loop :for rect :in rects
       :do (with-slots (x y width height operation) rect
	     (case operation
	       (:add (when (pointer-over-p (- pointer-x global-x) (- pointer-y global-y) x y width height)
		       (return-from pointer-over-input-region-p t)))
	       (:subtract (when (pointer-over-p (- pointer-x global-x) (- pointer-y global-y) x y width height)
			    (return-from pointer-over-input-region-p nil))))))
    nil))

(defmethod pointer-over-surface-p ((surface isurface) pointer-x pointer-y)
  (with-slots (x y wl-surface) surface
    (with-slots (width height) wl-surface
      (pointer-over-p pointer-x pointer-y x y width height))))

#|
(defmethod pointer-over-surface-p ((surface ulubis-cursor) pointer-x pointer-y)
  nil)
|#

(defun surface-under-pointer (x y view)
  (find-if (lambda (surface)
	     (or (and (pointer-over-surface-p surface x y) ;; pointer is over client and has no input-region
		      (not (input-region (wl-surface surface))))
		 (and (pointer-over-surface-p surface x y) ;; or pointer is over client, has an input-region, and pointer is over input-region
		      (input-region (wl-surface surface))
		      (pointer-over-input-region-p x y surface))))
	   (surfaces view)))      

;; TODO: support input-region
#|
(defun surface-quadrant (pointer-x pointer-y surface)
  (with-slots (x y width height input-region) surface
    (let ((half-width (round (/ width 2)))
	  (half-height (round (/ height 2))))
      (cond
	((and (<= pointer-x (+ x half-width)) (<= pointer-y (+ y half-height)))
	 :top-left)
	((and (>= pointer-x (+ x half-width)) (<= pointer-y (+ y half-height)))
	 :top-right)
	((and (>= pointer-x (+ x half-width)) (>= pointer-y (+ y half-height)))
	 :bottom-right)
	((and (<= pointer-x (+ x half-width)) (>= pointer-y (+ y half-height)))
	 :bottom-left)))))
|#      

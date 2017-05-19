
(in-package :ulubis.panels)

(defclass panel-container ()
  ((top-panel :accessor top-panel :initarg :top-panel :initform t)
   (bottom-panel :accessor bottom-panel :initarg :bottom-panel :initform t))
  (:documentation "Each slot can be
T - delegate panel (mode -> view -> compositor)
nil - don't render any panel
PANEL instance - render the instance"))

(defgeneric delegate (instance)
  (:documentation "Get next object in hierarchy. NIL if it's the root"))
(defmethod delegate ((instance panel-container))
  nil)

(defclass panel (ulubis.cairo:surface)
  ((height :accessor height :initarg :height :initform 15)
   (left-bars :accessor left-bars :initarg :left-bars :initform nil)
   (right-bars :accessor right-bars :initarg :right-bars :initform nil)))

(defmethod initialize-instance :after ((panel panel) &key)
  (setf (ulubis.cairo:draw-func panel) (lambda () (update panel))))

(defun paint-surface-into-rectangle (surface x y width height)
  (cl-cairo2:rectangle x y width height)
  (cl-cairo2:clip)
  (cl-cairo2:translate x y)
  (cl-cairo2:set-source-surface surface 0 0)
  (cl-cairo2:paint)
  (cl-cairo2:reset-clip)
  (cl-cairo2:translate (- x) (- y)))

(defmethod height ((panel (eql nil)))
  0)

(defgeneric update (thing))

(defmethod update ((panel panel))
  "Just put all bars where they belong"
  (flet ((active-color ()
           (cl-cairo2:set-source-rgb 0.506 0.396 0.310))
         (inactive-color ()
           (cl-cairo2:set-source-rgb 0.659 0.616 0.588))
         (black-color ()
           (cl-cairo2:set-source-rgb 0 0 0)))
    (with-slots (height clear-color left-bars right-bars) panel
      ;; Turn it upside-down (because it will be rendered upside-down in ulubis.lisp >_>
      (cl-cairo2:scale 1 -1)
      (cl-cairo2:translate 0 (- (height panel)))
      ;; Fill background
      (cl-cairo2:set-line-width 1)
      (inactive-color)
      (cl-cairo2:paint)
      ;; Draw border
      (active-color)
      (cl-cairo2:rectangle 0.5 0.5 (- (width panel) 0.5) (- (height panel) 0.5))
      (cl-cairo2:stroke)
      ;; Left bars
      (let ((cursor 0))
        (loop :for bar :in left-bars
           :do (unless (= (ulubis.cairo:height panel)
                          (ulubis.cairo:height bar))
                 (error "Bar height doesn't match panel"))
           :do (paint-surface-into-rectangle (ulubis.cairo:cairo-surface bar) cursor 0 (width bar) height)
           :do (incf cursor (width bar))))
      ;; Right bars
      (let ((cursor (ulubis.cairo:width panel)))
        (loop :for bar :in right-bars
           :do (unless (= (ulubis.cairo:height panel)
                          (ulubis.cairo:height bar))
                 (error "Bar height doesn't match panel"))
           :do (decf cursor (width bar))
           :do (paint-surface-into-rectangle (ulubis.cairo:cairo-surface bar) cursor 0 (width bar) height))))))


(defclass bar (ulubis.cairo:surface)
  ())

(defmethod make-instance :after ((bar bar) &key)
  (update bar))

(defgeneric get-width (bar))
(defmethod get-width ((bar bar))
  (error "Must implement get-width on BAR"))

(defmethod update ((bar bar))
  "Resize the surface then redraw the contents"
  (ulubis.cairo:resize bar (get-width bar) (ulubis.cairo:height bar))
  (ulubis.cairo:redraw bar)
  (ulubis:request-render))

;;; Bars

(defclass views-bar (bar)
  ())

(defvar *views-bars* nil)

(defmethod initialize-instance :after ((bar views-bar) &key)
  (push bar *views-bars*)
  (setf (draw-func bar) #'draw-view-bar)
  (update bar))

(defun draw-view-bar ()
  (flet ((active-color ()
           (cl-cairo2:set-source-rgb 0.506 0.396 0.310))
         (inactive-color ()
           (cl-cairo2:set-source-rgb 0.659 0.616 0.588))
         (black-color ()
           (cl-cairo2:set-source-rgb 0 0 0)))
    (let ((views (ulubis:views ulubis:*compositor*))
          (current-view (ulubis:current-view ulubis:*compositor*)))
      ;; Backgrounds
      (loop :for view :in views
         :for i :from 0
         :do (progn
               (if (eq view current-view)
                   (active-color)
                   (inactive-color))
               (cl-cairo2:rectangle (+ 0.5 (* i 20)) 0.5 19 14.5)
               (cl-cairo2:fill-path)
               (cl-cairo2:rectangle (+ 0.5 (* i 20)) 0.5 19 14.5)
               (if (eq view current-view)
                   (black-color)
                   (active-color))
               (cl-cairo2:stroke)))
      ;; Labels
      (cl-cairo2:set-font-size 13)
      (cl-cairo2:set-line-width 1)
      (black-color)
      (loop :for view :in views
         :for i :from 0
         :do (progn
               (cl-cairo2:move-to (+ 6 (* i 20)) 12)
               (cl-cairo2:show-text (format nil "~A" (+ i 1))))))))

(defmethod (setf ulubis:views) :after (new-value compositor)
  (mapc #'update *views-bars*))

(defmethod (setf ulubis:current-view) :after (new-value compositor)
  (mapc #'update *views-bars*))

(defmethod get-width ((bar views-bar))
  (* 20 (length (ulubis:views ulubis:*compositor*))))

(defvar *status-bars* nil)

(defclass status-bar (bar)
  ((text-func :accessor text-func :initarg :text-func :initform (lambda ()))
   (text :accessor text :initform "")
   (last-update-time :accessor last-update-time :initform (ulubis::get-milliseconds))))

(defmethod initialize-instance :after ((bar status-bar) &key)
  (push bar *status-bars*)
  (setf (draw-func bar)
        (lambda ()
          (draw-status-bar bar))))

(defun draw-status-bar (bar)
  (flet ((active-color ()
           (cl-cairo2:set-source-rgb 0.506 0.396 0.310))
         (inactive-color ()
           (cl-cairo2:set-source-rgb 0.659 0.616 0.588))
         (black-color ()
           (cl-cairo2:set-source-rgb 0 0 0)))
    (active-color)
    (cl-cairo2:rectangle 0 0 (width bar) (height bar))
    (cl-cairo2:stroke)
    (black-color)
    (cl-cairo2:move-to 4 11)
    (cl-cairo2:show-text (text bar))))

(defmethod update :around ((bar status-bar))
  (with-slots (last-update-time) bar
    (when (< (+ last-update-time 1000) (ulubis::get-milliseconds))
      (incf last-update-time 1000)
      (call-next-method))))

(defmethod get-width ((bar status-bar))
  (with-slots (text) bar
    (setf text (funcall (text-func bar)))
    (multiple-value-bind (x-bearing y-bearing text-width)
        (cl-cairo2:text-extents text (ulubis.cairo::cairo-context bar))
      (declare (ignore x-bearing y-bearing))
      (+ text-width 10))))

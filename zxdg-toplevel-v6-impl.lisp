
(in-package :ulubis)

(declaim (optimize (debug 3)))

(def-wl-callback set-title (client toplevel (title :string))
  (setf (title toplevel) title)
  (format t "Setting title of ~A to ~A~%" toplevel title))

(def-wl-callback move (client toplevel (seat :pointer) (serial :uint32))
  (when (allow-move? (current-mode *compositor*) toplevel)
    (setf (moving-surface *compositor*) (make-move-op :surface toplevel
                                                      :surface-x (x toplevel)
                                                      :surface-y (y toplevel)
                                                      :pointer-x (pointer-x *compositor*)
                                                      :pointer-y (pointer-y *compositor*)))))

(def-wl-callback zxdg-toplevel-destroy (client toplevel)
  (setf (role (wl-surface toplevel)) nil)
  (remove-surface toplevel *compositor*)
  (request-render))

(def-wl-delete zxdg-toplevel-delete (toplevel)
  (when toplevel
    (setf (role (wl-surface toplevel)) nil)
    (remove-surface toplevel *compositor*)
    (request-render)))

(defimplementation zxdg-toplevel-v6 (isurface ianimatable)
  ((:move move)
   (:destroy zxdg-toplevel-destroy)
   (:set-title set-title))
  ((zxdg-surface-v6 :accessor zxdg-surface-v6 :initarg :zxdg-surface-v6 :initform nil)
   (requested-size :accessor requested-size :initarg :requested-size :initform (list 0 0))
   (requested-max :accessor requested-max :initform nil)
   (requested-full :accessor requested-full :initform nil)
   (title :accessor title :initform "")))

(defmethod activate ((surface zxdg-toplevel-v6) active-surface mods)
  (call-next-method)
  (with-wl-array array
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (configure-to-requested surface array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0))
  surface)

(defmethod deactivate ((surface zxdg-toplevel-v6))
  (call-next-method)
  (with-wl-array array
    (configure-to-requested surface array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0)))

(defmethod resize ((surface zxdg-toplevel-v6) width height time
                   &key (activate? t) maximize fullscreen)
  (setf (requested-size surface) (list (round width) (round height)))
  (with-wl-array array
    (setf (requested-max surface) maximize)
    (setf (requested-full surface) fullscreen)
    (when maximize
      (setf (mem-aref (wl-array-add array 4) :int32) 1))
    (when fullscreen
      (setf (mem-aref (wl-array-add array 4) :int32) 2))
    (setf (mem-aref (wl-array-add array 4) :int32) 3)
    (when activate?
      (setf (mem-aref (wl-array-add array 4) :int32) 4))
    (zxdg-toplevel-v6-send-configure (->resource surface) (round width) (round height) array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0)))

(defmethod configure-to-requested ((surface zxdg-toplevel-v6) array)
  (destructuring-bind (width height) (requested-size surface)
    (when (requested-max surface)
      (setf (mem-aref (wl-array-add array 4) :int32) 1))
    (when (requested-full surface)
      (setf (mem-aref (wl-array-add array 4) :int32) 2))
    (zxdg-toplevel-v6-send-configure (->resource surface) width height array)))

(defmethod window-geometry ((toplevel zxdg-toplevel-v6))
  (window-geometry (zxdg-surface-v6 toplevel)))

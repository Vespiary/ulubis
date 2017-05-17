
(in-package :ulubis)

(def-wl-callback get-toplevel (client zxdg-surface (id :uint32))
  (let ((toplevel (make-zxdg-toplevel-v6 client 1 id :delete-fn (callback zxdg-toplevel-delete))))
    ;; Save the xdg-surface object so that configure events can be sent
    (setf (zxdg-surface-v6 toplevel) zxdg-surface)
    ;; Surface role now becomes xdg-toplevel
    (setf (role (wl-surface zxdg-surface)) toplevel)
    ;; Save the wl-surface associated with the toplevel
    (setf (wl-surface toplevel) (wl-surface zxdg-surface))
    (push toplevel (surfaces (current-view *compositor*)))
    (first-configure (current-mode *compositor*) toplevel)))

(defstruct (window-geometry
             (:constructor make-window-geometry (x y w h)))
  x y w h)

(def-wl-callback set-window-geometry (client zxdg-surface (x :int32) (y :int32) (w :int32) (h :int32))
  (setf (window-geometry zxdg-surface) (make-window-geometry x y w h)))

(defimplementation zxdg-surface-v6 (isurface)
  ((:get-toplevel get-toplevel)
   (:set-window-geometry set-window-geometry))
  ((window-geometry :accessor window-geometry :initform (make-window-geometry 0 0 1 1))))

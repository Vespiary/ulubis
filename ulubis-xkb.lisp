
(in-package :ulubis.xkb)

(defvar *context* nil)

(defclass state ()
  ((xkb-state :reader xkb-state :initform nil)
   (xkb-keymap :initform nil)
   (depressed-keys :initform nil)
   (last-pressed-key :reader last-pressed-key :initform nil)))

(defmethod initialize-instance ((state state) &key (rules "") (model "") (layout "us") (variant "") (options "") &allow-other-keys)
  (call-next-method)
  (ensure-context)
  (with-slots (xkb-keymap xkb-state) state
    (setf xkb-keymap (xkb:new-keymap-from-names *context* rules model layout variant options))
    (setf xkb-state (xkb:xkb-state-new xkb-keymap))))

(defun ensure-context ()
  (unless *context*
    (setf *context* (xkb:xkb-context-new 0))))

(defun free (state)
  (xkb:xkb-state-unref (xkb-state state)))

(defun update-key (ulubis-xkb-state keycode state)
  (incf keycode 8)
  (with-slots (xkb-state depressed-keys last-pressed-key) ulubis-xkb-state
    (setf last-pressed-key (xkb:xkb-state-key-get-one-sym xkb-state keycode))
    (if (and (= state 1)
             (member keycode depressed-keys))
        (progn
          ;;(format t "!!! Not updating key state~%")
          )
        (xkb:xkb-state-update-key xkb-state keycode state))
    (if (= state 1)
        (push keycode depressed-keys)
        (setf depressed-keys (delete keycode depressed-keys)))))

(defun serialize-mods (state)
  (list (xkb:xkb-state-serialize-mods (xkb-state state) xkb:+xkb-state-mods-depressed+)
        (xkb:xkb-state-serialize-mods (xkb-state state) xkb:+xkb-state-mods-latched+)
        (xkb:xkb-state-serialize-mods (xkb-state state) xkb:+xkb-state-mods-locked+)
        (xkb:xkb-state-serialize-layout (xkb-state state) xkb:+xkb-state-layout-effective+)))

(defun state-keymap-name (state)
  (xkb:xkb-keymap-get-as-string (xkb:xkb-state-get-keymap (xkb-state state)) 1))

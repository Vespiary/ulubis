
(in-package :ulubis.xkb)

(defvar *context* nil)
(defvar *states* (make-hash-table :test 'equal))

(defclass state ()
  ((xkb-state :reader xkb-state :initform nil)
   (xkb-keymap :initform nil)
   (depressed-keys :initform nil)
   (last-pressed-key :reader last-pressed-key :initform nil)))

(defun get-state (&key (layout "us") (variant "") (options ""))
  (let* ((key (list layout variant options))
         (state (gethash key *states*)))
    (unless state
      (setf (gethash key *states*) (make-instance 'state
                                                  :layout layout
                                                  :variant variant
                                                  :options options)))
    (gethash key *states*)))

(defmethod initialize-instance ((state state) &key (layout "us") (variant "") (options "") &allow-other-keys)
  (call-next-method)
  (ensure-context)
  (with-slots (xkb-keymap xkb-state) state
    (setf xkb-keymap (xkb:new-keymap-from-names *context* "" "" layout variant options))
    (setf xkb-state (xkb:xkb-state-new xkb-keymap))))

(defun ensure-context ()
  (unless *context*
    (setf *context* (xkb:xkb-context-new 0))))

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

(defun serialize-mods (state components)
  (xkb:xkb-state-serialize-mods (xkb-state state) components))

(defun serialize-layout (state components)
  (xkb:xkb-state-serialize-layout (xkb-state state) components))

(defun state-keymap-name (state)
  (xkb:xkb-keymap-get-as-string (xkb:xkb-state-get-keymap (xkb-state state)) 1))

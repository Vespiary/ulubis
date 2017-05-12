
(in-package :ulubis.config)

(defun set-keymap (r m l v o)
  (ulubis::set-keymap ulubis::*compositor*
                      r m l v o))

(defun enable-debugger ()
  (setf ulubis::*enable-debugger* t))

(defun next-view ()
  (ulubis:nth-view (+ (position (ulubis::current-view ulubis::*compositor*) (ulubis::views ulubis::*compositor*))
                      1)))
(defun prev-view ()
  (ulubis:nth-view (- (position (ulubis::current-view ulubis::*compositor*) (ulubis::views ulubis::*compositor*))
                      1)))

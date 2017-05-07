
(in-package :ulubis.config)

(defun set-keymap (r m l v o)
  (ulubis::set-keymap ulubis::*compositor*
                      r m l v o))

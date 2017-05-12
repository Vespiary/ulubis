;;;; package.lisp

(defpackage :ulubis
  (:use :common-lisp
	:cffi
	;;:cepl
	;;:varjo-lang
	:wayland-util
	:wayland-server-core
	:wayland-server-protocol
	:xdg-shell-server-protocol
	:zxdg-shell-v6-server-protocol
	:waylisp
	:ulubis-backend
	:animation)
   ;; Interface for modes
  (:export #:mode
           #:defmode
           #:init-mode
           #:render
           #:first-commit
           #:mouse-motion-handler
           #:mouse-button-handler))

(defpackage :ulubis.wmii
  (:use :common-lisp
        :waylisp
        :ulubis)
  (:export #:wmii-mode))


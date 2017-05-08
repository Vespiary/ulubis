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
	:animation))

(defpackage :ulubis.wmii
  (:use :common-lisp)
  (:export :layout))

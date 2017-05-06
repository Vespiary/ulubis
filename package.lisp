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
  (:export #:push-view
           #:desktop-mode
           #:defkeybinding
           #:set-keymap
           #:*compositor*
           #:alt
           #:shift
           #:ctrl
           #:gui))

(defpackage :ulubis.xkb
  (:use :common-lisp)
  (:export :get-state
           :state-keymap-name
           :update-key
           :last-pressed-key
           :serialize-mods
           :serialize-layout))


(defpackage :ulubis.config
  (:use :common-lisp)
  (:import-from :ulubis
                #:push-view
                #:desktop-mode
                #:defkeybinding
                #:*compositor*
                #:alt
                #:shift
                #:ctrl
                #:gui))

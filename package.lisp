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
           #:mouse-button-handler
   ;; Interface for configs
           #:push-view
           #:nth-view
           #:desktop-mode
           #:defkeybinding
           #:alt
           #:shift
           #:ctrl
           #:gui
           ))

(defpackage :ulubis.wmii
  (:use :common-lisp
        :waylisp
        :ulubis)
  (:export #:wmii-mode))

(defpackage :ulubis.xkb
  (:use :common-lisp)
  (:export :state
           :free
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
                #:alt
                #:shift
                #:ctrl
                #:gui))

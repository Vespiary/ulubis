;;;; package.lisp

(defpackage :ulubis.cairo
  (:use :common-lisp)
  (:export #:surface
           #:draw-func
           #:redraw
           #:resize
           #:cairo-surface
           #:width
           #:height))

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
  (:export #:*compositor*
           #:texture-of
           #:request-render
           ;; Interface for configs
           #:push-view
           #:nth-view
           #:views
           #:current-view
           #:desktop-mode
           #:wmii-mode
           #:defkeybinding
           #:alt
           #:shift
           #:ctrl
           #:gui
           ))

(defpackage :ulubis.panels
  (:use :common-lisp
        :ulubis.cairo)
  (:export #:panel-container
           #:top-panel
           #:bottom-panel
           #:render-panels
           #:panel
           #:delegate
           #:update
           #:clear-color
           #:height
           #:left-bars
           #:right-bars
           #:bar
           #:get-width
           #:views-bar
           #:status-bar
           ))

(defpackage :ulubis.wmii
  (:use :common-lisp
        :waylisp
        :ulubis)
  (:export #:layout
           #:surfaces
           #:width
           #:height
           #:add-surface
           #:remove-surface
           #:reposition-all-windows
           #:set-active-surface
           #:move-cursor-up
           #:move-cursor-down
           #:move-cursor-left
           #:move-cursor-right
           #:move-window-up
           #:move-window-down
           #:move-window-left
           #:move-window-right
           #:resize-column
           #:set-column-mode
           #:toggle-fullscreen
           ))

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
                #:wmii-mode
                #:defkeybinding
                #:alt
                #:shift
                #:ctrl
                #:gui)
  (:import-from :ulubis.panels
                #:views-bar
                #:status-bar))

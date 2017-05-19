
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

(defmacro define-panel (name (&key (height 15)) &body clauses)
  (alexandria:once-only (height)
    (let (left spacer right)
      (dolist (clause clauses)
        (cond ((eq clause :space)
               (setf spacer t))
              (spacer
               (push clause right))
              (t
               (push clause left))))
      `(defparameter ,name (make-instance 'ulubis.panels:panel
                                          :height ,height
                                          :width (ulubis::desktop-width)
                                          :left-bars (list
                                                      ,@(reverse
                                                         (loop :for clause :in left
                                                            :collect `(make-instance ',(first clause) ,@(rest clause)
                                                                                     :width 1
                                                                                     :height ,height))))
                                          :right-bars (list
                                                       ,@(loop :for clause :in right
                                                            :collect `(make-instance ',(first clause) ,@(rest clause)
                                                                                     :width 1
                                                                                     :height ,height))))))))

(defun set-panels (&key top bottom)
  (setf (ulubis.panels:top-panel ulubis:*compositor*) top)
  (setf (ulubis.panels:bottom-panel ulubis:*compositor*) bottom))

(defun run-program (program)
  (uiop:launch-program program))

(defun run-program-for-output (program &key keep-newlines)
  (let ((string (with-output-to-string (str)
                  (uiop:wait-process (uiop:launch-program program :output str)))))
    (unless keep-newlines
      (string-trim '(#\Newline) string))))

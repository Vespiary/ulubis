
(in-package :ulubis.wmii)

(declaim (optimize debug))

(defparameter *header-height* 15)
(defparameter *layout* nil)

(defclass layout ()
  ((columns :accessor columns
            :initform nil)
   (active-column :accessor active-column
                  :initform nil)
   (reposition-callback :accessor reposition-callback
                        :initarg :reposition-callback
                        :initform (lambda (surface top bottom left right &rest args)
                                    (error "Reposition callback not set")))
   (activate-callback :accessor activate-callback
                      :initarg :activate-callback
                      :initform (lambda (surface)
                                  (error "Activate callback not set")))
   (width :accessor width
          :initarg :width
          :initform 0)
   (height :accessor height
           :initarg :height
           :initform 0)))

(defclass column ()
  ((mode :accessor mode
         :initform :stack
         :documentation ":STACK, :EVEN, :TABS")
   (windows :accessor windows
            :initform nil)
   (active-window :accessor active-window
                  :initform nil)
   (width :accessor width
          :initform 1)))

(defclass window ()
  ((fullscreen :accessor fullscreen
               :initform nil)
   (surface :reader surface
            :initarg :surface
            :initform nil)))

;;; Utils
(defun list-prev (item list)
  (when (= (length list) 1)
    (return-from list-prev item))
  (when (>= (length list) 2)
    (let ((i (position item list)))
      (if (= i 0)
          (elt list (- (length list) 1)) 
          (elt list (- i 1))))))

(defun list-next (item list)
  (when (= (length list) 1)
    (return-from list-next item))
  (when (>= (length list) 2)
    (let ((i (position item list)))
      (if (= i (- (length list) 1))
          (elt list 0) 
          (elt list (+ i 1))))))

(defun insert-after (list at item)
  (when (or (null list) (null at))
    (error "LIST and AT must both be non-null"))
  (push item (cdr (member at list)))
  list)
(define-modify-macro notf () not
  "Set `place` to `(not place)` in-place")

(define-modify-macro insert-afterf (at item) insert-after
  "Insert `item` after `at` in-place")

(defun call-reposition-window (surface top bottom left right &rest args)
  (when (typep surface 'window)
    (error "Wrapped window"))
  (apply (reposition-callback *layout*) surface
         top bottom left right
         args))

(defgeneric call-activate-surface (surface))
(defmethod call-activate-surface ((window window))
  (call-activate-surface (surface window)))
(defmethod call-activate-surface (surface)
  (funcall (activate-callback *layout*)
           surface))

;;; layout
(defmethod active-window ((layout layout))
  (let ((active-column (active-column layout)))
    (when active-column
      (active-window active-column))))

(defun add-surface (surface layout)
  (let ((window (make-instance 'window :surface surface))
        (*layout* layout)
        (created-column nil))
    (with-slots (columns active-column) layout
      (unless columns
        (create-column layout)
        (setf created-column t))
      (add-window window active-column)
      (if created-column
          (reposition-all-windows)
          (reposition-windows active-column))
      (call-activate-surface (surface (active-window active-column))))))

(defun create-column (layout &key end)
  (let ((new-column (make-instance 'column)))
    (with-slots (columns active-column) layout
      (cond (columns
             (if end
                 (setf columns (append columns (list new-column)))
                 (push new-column columns)))
            (t
             (setf columns (list new-column))
             (setf active-column new-column))))))

(defgeneric remove-window (window target))
(defgeneric remove-surface (surface target))

(defmethod remove-surface (surface (target layout))
  (let ((*layout* target))
    (with-slots (columns active-column) target
      (dolist (column columns)
        (remove-surface surface column)
        (unless (windows column)
          (remove-column column target)
          (when columns
            (call-activate-surface (active-window active-column))))))))

(defun remove-column (column layout)
  (with-slots (columns active-column) layout
    (when (eq column active-column)
      (if (= 1 (length columns))
          (setf active-column nil)
          (let ((i (position active-column columns)))
            (if (= i 0)
                (setf active-column (second columns))
                (setf active-column (elt columns (- i 1)))))))
    (setf columns (delete column columns))
    (reposition-all-windows)))

(defun move-cursor-left (layout)
  (let ((*layout* layout))
    (with-slots (columns active-column) layout
      (setf active-column (list-prev active-column columns))
      (call-activate-surface (active-window active-column)))))

(defun move-cursor-right (layout)
  (let ((*layout* layout))
    (with-slots (columns active-column) layout
      (setf active-column (list-next active-column columns))
      (call-activate-surface (active-window active-column)))))

(defgeneric move-cursor-up (target))

(defmethod move-cursor-up ((layout layout))
  (let ((*layout* layout))
    (with-slots (active-column) layout
      (when active-column
        (move-cursor-up active-column)
        (reposition-windows active-column)
        (call-activate-surface (active-window active-column))))))

(defgeneric move-cursor-down (target))

(defmethod move-cursor-down ((layout layout))
  (let ((*layout* layout))
    (with-slots (active-column) layout
      (when active-column
        (move-cursor-down active-column)
        (reposition-windows active-column)
        (call-activate-surface (active-window active-column))))))

(defun move-window-left (layout)
  (let ((*layout* layout)
        (reposition-all nil))
    (with-slots (columns active-column) layout
      (when (= (length columns) 0)
        (return-from move-window-left))
      (when (eq active-column (first columns))
        (when (= (length (windows active-column)) 1)
          (return-from move-window-left))
        (create-column layout)
        (setf reposition-all t))
      (let ((other-column (list-prev active-column columns)))
        (add-window (active-window active-column) other-column)
        (remove-window (active-window active-column) active-column)
        (when (null (windows active-column))
          (remove-column active-column layout)
          (setf reposition-all t))
        (setf active-column other-column)
        (if reposition-all
            (reposition-all-windows)
            (reposition-windows active-column))
        (call-activate-surface (active-window active-column))))))

(defun move-window-right (layout)
  (let ((*layout* layout)
        (reposition-all nil))
    (with-slots (columns active-column) layout
      (when (= (length columns) 0)
        (return-from move-window-right))
      (when (eq active-column (first (last columns)))
        (when (= (length (windows active-column)) 1)
          (return-from move-window-right))
        (create-column layout :end t)
        (setf reposition-all t))
      (let ((other-column (list-next active-column columns)))
        (add-window (active-window active-column) other-column)
        (remove-window (active-window active-column) active-column)
        (when (null (windows active-column))
          (remove-column active-column layout)
          (setf reposition-all t))
        (setf active-column other-column)
        (if reposition-all
            (reposition-all-windows)
            (reposition-windows active-column))
        (call-activate-surface (active-window active-column))))))

(defgeneric move-window-up (target))

(defmethod move-window-up ((layout layout))
  (let ((*layout* layout))
    (with-slots (active-column) layout
      (when active-column
        (move-window-up active-column)
        (reposition-windows active-column)))))

(defgeneric move-window-down (target))

(defmethod move-window-down ((layout layout))
  (let ((*layout* layout))
    (with-slots (active-column) layout
      (when active-column
        (move-window-down active-column)
        (reposition-windows active-column)))))

(defun resize-column (layout side by)
  "Side is either :left or :right"
  (let ((*layout* layout))
    (with-slots (active-column columns) layout
      (let ((i (position active-column columns)))
        (case side
          (:left (incf i -1))
          (:right (incf i 1))
          (t (error "Side must be either :left or :right")))
        (when (< i 0)
          (return-from resize-column))
        (let ((other-column (nth i columns)))
          (unless other-column
            (return-from resize-column))
          (when (and (> (+ (width active-column) by) 0)
                     (> (- (width other-column) by) 0))
            (incf (width active-column) by) 
            (decf (width other-column) by)
            (reposition-windows active-column)
            (reposition-windows other-column)))))))

(defun set-column-mode (layout mode)
  (let ((*layout* layout))
    (with-slots (active-column) layout
      (setf (mode active-column) mode)
      (reposition-windows active-column))))

(defun toggle-fullscreen (layout)
  (let ((*layout* layout)
        (active-window (active-window (active-column layout))))
    (notf (fullscreen active-window))
    (if (fullscreen active-window)
        (reposition-to-fullscreen active-window)
        (reposition-windows (active-column layout)))))

(defgeneric set-active-surface (surface target))
(defmethod set-active-surface (surface (target layout))
  (let ((*layout* target))
    (dolist (column (columns target))
      (when (set-active-surface surface column)
        (setf (active-column target) column)
        (reposition-windows column))
      (return-from set-active-surface t))))

(defgeneric surfaces (container))

(defmethod surfaces ((layout layout))
  (mapcan #'surfaces (columns layout)))

(defun total-column-widths (layout)
  (apply #'+ (mapcar #'width (columns layout))))

(defun reposition-all-windows (&optional (layout *layout*))
  (when (null layout)
    (error "LAYOUT is NIL"))
  (let ((*layout* layout))
    (mapc #'reposition-windows (columns layout))))

;;; column
(defmethod move-cursor-up ((column column))
  (with-slots (windows active-window) column
    (setf active-window (list-prev active-window windows))))

(defmethod move-cursor-down ((column column))
  (with-slots (windows active-window) column
    (setf active-window (list-next active-window windows))))

(defmethod move-window-up ((column column))
  (unless (eq (active-window column)
              (first (windows column)))
    (switch-windows column #'list-prev)))

(defmethod move-window-down ((column column))
  (unless (eq (active-window column)
              (first (last (windows column))))
    (switch-windows column #'list-next)))

(defun switch-windows (column func)
  (with-slots (active-window windows) column
    (when (> (length windows) 1)
      (let ((next-window (funcall func active-window windows)))
        (setf windows (remove active-window windows))
        (if (eq (first windows) next-window)
            (push active-window windows)
            (insert-afterf windows (list-prev next-window windows) active-window))))))

(defun add-window (window column)
  (with-slots (active-window windows) column
    (cond ((null windows)
           (push window windows))
          (t
           (insert-afterf windows active-window window)))
    (setf active-window window)))

(defmethod remove-surface (item (target column))
  (let ((window (find item (windows target) :key #'surface)))
    (when window
      (remove-window window target))))

(defmethod remove-window ((item window) (target column))
  (with-slots (active-window windows) target
    (let ((new-active-window (cond ((= (length windows) 1)
                                    nil)
                                   ((eq active-window (first windows))
                                    (second windows))
                                   (t
                                    (list-prev item windows))))) 
      (setf windows (remove item windows))
      (when (eq active-window item)
        (setf active-window new-active-window))
      (reposition-windows target)
      (when active-window
        (call-activate-surface (surface active-window))))))

(defmethod surfaces ((column column))
  (mapcar #'surface (windows column)))

(defmethod set-active-surface (surface (target column))
  (dolist (window (windows target))
    (when (set-active-surface surface window)
      (setf (active-window target) window)
      (call-activate-surface surface)
      (return-from set-active-surface t))))

(defun column-bounds (column)
  (let ((screen-width (width *layout*))
        (total-width (total-column-widths *layout*))
        (left 0)
        (right 0))
    (loop :for item :in (columns *layout*)
       :do (if (eq item column)
               (progn
                 (setf right (+ left -1 (* (width item) screen-width (/ 1 total-width))))
                 (return-from column-bounds (values left right)))
               (incf left (* (width item) screen-width (/ 1 total-width))))))
  (error "Column not found"))

(defun reposition-windows (column)
  (multiple-value-bind (left right) (column-bounds column)
    (let ((top -1)
          (bottom -1))
      (loop :for window :in (windows column)
         :do (ecase (mode column)
               (:stack (setf top (+ 1 bottom))
                       (setf bottom (+ top
                                       -1
                                       (if (eq window (active-window column))
                                           (- (height *layout*)
                                              (* *header-height*
                                                 (- (length (windows column))
                                                    1)))
                                           *header-height*)))
                       (if (eq window (active-window column))
                           (call-reposition-window (surface window)
                                                   top bottom left right)
                           (call-reposition-window (surface window)
                                                   top bottom left right
                                                   :render-surface nil)))
               (:even (setf top (+ bottom 1))
                      (setf bottom (+ top (/ (height *layout*) (length (windows column)))))
                      (call-reposition-window (surface window)
                                              top bottom left right))
               (:tabs (if (eq window (active-window column))
                          (call-reposition-window (surface window)
                                                  0 (height *layout*) left right
                                                  :tabs (surfaces column))
                          (call-reposition-window (surface window)
                                                  0 0 0 0
                                                  :visible nil))))))))

;;; window

(defmethod set-active-surface (surface (target window))
  (eq surface (surface target)))

(defun reposition-to-fullscreen (window)
  (call-reposition-window (surface window)
                          0 (ulubis::screen-height ulubis:*compositor*)
                          0 (ulubis::screen-width ulubis:*compositor*)
                          :fullscreen t))


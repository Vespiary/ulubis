
(in-package ulubis.wmii)

(defparameter *header-height* 15)

(defclass layout ()
  ((columns :accessor columns
            :initform nil)
   (active-column :accessor active-column
                  :initform nil)
   (width :accessor width
          :initarg :width
          :initform 0)
   (height :accessor height
           :initarg :height
           :initform 0)))

(defclass column ()
  ((mode :accessor mode
         :initform :stack)
   (windows :accessor windows
            :initform nil)
   (active-window :accessor active-window
                  :initform nil)
   (width :accessor width
          :initform 1)))

(defclass window ()
  ((floating :accessor floating
             :initform nil)
   (fullscreen :accessor fullscreen
               :initform nil)
   (surface :reader surface
            :initarg :surface
            :initform nil)))

(define-modify-macro notf () not
  "Set `place` to `(not place)` in-place")

(define-modify-macro insert-afterf (at item) insert-after
  "Insert `item` after `at` i-place")

;;; layout
(defmethod active-window ((layout layout))
  (let ((active-column (active-column layout)))
    (when active-column
      (active-window active-column))))

(defgeneric add-window (surface target))

(defmethod add-window (surface (target layout))
  (let ((window (make-instance 'window :surface surface)))
    (with-slots (columns active-column) target
      (unless columns
        (create-column target))
      (add-window window active-column))))

(defun create-column (layout &key end)
  (let ((new-column (make-instance 'column)))
    (with-slots (columns active-column) layout
      (if columns
          (if end
              (setf columns (append columns (list new-column)))
              (push new-column columns))
          (setf columns (list new-column))))
    (recalc-columns-widths layout)))

(defgeneric remove-window (surface target))

(defmethod remove-window (surface (target layout))
  (dolist (column (columns target))
    (remove-window surface column)
    (when (null (windows column))
      (remove-column column layout))))

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
    (recalc-columns-widths layout)))

(defun move-cursor-left (layout)
  (with-slots (columns active-column) layout
    (setf active-column (list-prev columns active-column))))

(defun move-cursor-right (layout)
  (with-slots (columns active-column) layout
    (setf active-column (list-next columns active-column))))

(defgeneric move-cursor-up (target))

(defmethod move-cursor-up ((layout layout))
  (with-slots (active-column) layout
    (when active-column
      (move-cursor-up active-column))))

(defgeneric move-cursor-down (target))

(defmethod move-cursor-down ((layout layout))
  (with-slots (active-column) layout
    (when active-column
      (move-cursor-down active-column))))

(defun move-window-left (layout)
  (with-slots (columns active-column) layout
    (when (= (length columns) 0)
      (return-from move-window-left))
    (when (eq active-column (first columns))
      (when (= (length (windows active-column)) 1)
        (return-from move-window-left))
      (create-column layout))
    (add-window (active-window active-column) (list-prev active-column columns))
    (remove-window (active-window active-column) active-column)
    (when (null (windows active-column))
      (remove-column active-column layout))))

(defun move-window-right (layout)
  (with-slots (columns active-column) layout
    (when (= (length columns) 0)
      (return-from move-window-right))
    (when (eq active-column (first (last columns)))
      (when (= (length (windows active-column)) 1)
        (return-from move-window-right))
      (create-column layout :end t))
    (add-window (active-window active-column) (list-next active-column columns))
    (remove-window (active-window active-column) active-column)
    (when (null (windows active-column))
      (remove-column active-column layout))))

(defgeneric move-window-up (target))

(defmethod move-window-up ((target layout))
  (with-slots (active-column) layout
    (when active-column
      (move-window-up active-column))))

(defgeneric move-window-down (target))

(defmethod move-window-down ((target layout))
  (with-slots (active-column) layout
    (when active-column
      (move-window-down active-column))))

(defun resize-column (layout side by)
  "Side is either :left or :right"
  (with-slots (active-column columns) layout
    (let ((i (position active-column columns)))
      (case side
        (:left (incf i -1))
        (:right (incf i 1))
        (t (error "Side must be either :left or :right")))
      (let ((other-column (nth i columns)))
        (unless other-column
          (return-from resize-column))
        (incf (width active-column) by) 
        (incf (width other-column) (- by))))))

(defun set-column-mode (layout mode)
  (setf (mode (active-column layout)) mode))

(defun toggle-fullscreen (layout)
  (notf (fullscreen (active-window (active-column layout)))))

;;; column
(defmethod move-cursor-up ((column column))
  (with-slots (windows active-window) column
    (setf active-window (list-prev windows active-window))))

(defmethod move-cursor-down ((column column))
  (with-slots (windows active-window) layout
    (setf active-window (list-next windows active-window))))

(defmethod move-window-up ((column column))
  (switch-windows column (active-window column) #'func-prev))

(defmethod move-window-down ((column column))
  (switch-windows column (active-window column) #'func-next))

(defun switch-windows (column win func)
  (with-slots (active-window windows) column
    (when (> (length windows) 1)
      (let ((next-window (funcall func active-window windows)))
        (setf windows (remove active-window windows))
        (if (eq (first windows) next-window)
            (push active-window windows)
            (insert-afterf windows (list-prev next-window windows) active-window))))))

(defmethod add-window ((item window) (target column))
  (with-slots (active-window windows) target
    (if (null windows)
        (push item windows)
        (insert-afterf windows active-window item))))

(defmethod remove-window ((item surface) (target column))
  (remove-window (find item (active-windows target) :key #'surface)
                 target))

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
        (setf active-window new-active-window)))))

;;; window

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
  (push item (cdr (member at list)))
  list)

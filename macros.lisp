
(in-package :ulubis)

(defmacro define-printer (class (obj-var stream-var) (&rest slots) &body body)
  "Defines how objects of CLASS should be printed. Makes sure all SLOTS are bound and executes BODY inside WITH-SLOTS for them."
  ;;; No need to use GENSYM since all arguments except body are supposed to be literal symbols
  ;;; And the macro doesn't introduce local variables
  `(defmethod print-object ((,obj-var ,class) ,stream-var)
     (print-unreadable-object (,obj-var ,stream-var :type t :identity nil)
       (if (and ,@(loop :for slot in slots
                     :collect `(slot-boundp ,obj-var ',slot)))
           (with-slots ,slots ,obj-var
             ,@body)
           (format ,stream-var "uninitialized")))))

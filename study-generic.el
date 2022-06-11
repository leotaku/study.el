;;; study-generic.el --- study.el support for generic viewers  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'eieio)
(require 'seq)

;;; Objects

(defclass study-client ()
  ((reference :initarg :reference :read-only t)))

(eieio-declare-slots :reference)

;;; Generic

(cl-defgeneric study-open ((class (subclass study-client)) uri page)
  (if-let ((client (study--best-viewer class uri)))
      (prog1 client
        (study-set-uri client uri)
        (study-set-page client page))
    (study-new class uri page)))

(cl-defgeneric study-new ((class (subclass study-client)) uri page)
  (let ((supported (seq-filter (lambda (it) (study-supports it uri)) (eieio-class-children class))))
    (study-new (seq-first supported) uri page)))

(cl-defgeneric study-instances ((class (subclass study-client)))
  (seq-mapcat #'study-instances (eieio-class-children class)))

(cl-defgeneric study-supports ((class (subclass study-client)) uri)
  (seq-some #'identity (seq-map (lambda (it) (study-supports it uri)) (eieio-class-children class))))

(cl-defgeneric study-get-uri ((client study-client)))

(cl-defgeneric study-get-page ((client study-client)))

(cl-defgeneric study-set-uri ((client study-client) uri))

(cl-defgeneric study-set-page ((client study-client) page-number))

(cl-defgeneric study-next-page ((client study-client))
  (study-set-page client (1+ (study-get-page client))))

(cl-defgeneric study-previous-page ((client study-client))
  (study-set-page client (1- (study-get-page client))))

(defun study--best-viewer (class uri)
  (when-let ((all (study-instances class))
             (filter (lambda (it) (study-supports (eieio-object-class it) uri)))
             (priority (lambda (it) (string-distance (study-get-uri it) uri))))
    (seq-first (seq-sort-by priority #'< (seq-filter filter all)))))


;;; History

(cl-defstruct study-history
  (offset 0)
  (elements nil))

(defun study-history-go (hist n)
  (when-let ((target (+ (study-history-offset hist) n))
             (elt (seq-elt (study-history-elements hist) target)))
    (when (>= target 0)
      (cl-incf (study-history-offset hist) n))))

(defun study-history-push (hist uri page)
  (let ((elem (cons uri page)))
    (unless (equal elem (seq-first (study-history-elements hist)))
      (setf (study-history-elements hist)
            (cons elem
                  (seq-drop (study-history-elements hist)
                            (study-history-offset hist))))
      (setf (study-history-offset hist) 0))))

(defun study-history-uri (hist)
  (car (seq-elt (study-history-elements hist)
                (study-history-offset hist))))

(defun study-history-page (hist)
  (cdr (seq-elt (study-history-elements hist)
                (study-history-offset hist))))

(provide 'study-generic)

;;; study-generic.el ends here

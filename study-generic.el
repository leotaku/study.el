;;; study-generic.el --- study.el support for generic viewers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 7 November 2021
;; Homepage: https://github.com/leotaku/study.el
;; Keywords: multimedia unix documents
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

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

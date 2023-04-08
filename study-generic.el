;;; study-generic.el --- Generic backend code for study.el clients  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 7 November 2021
;; Homepage: https://github.com/leotaku/study.el
;; Keywords: multimedia unix documents
;; Package-Version: 0.1.0

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

;; This package implements the abstract base class that clients
;; objects are expected to inherit from to integrate an external
;; application with Emacs.  Methods `study-new', `study-instances',
;; `study-supports', `study-get-uri' `study-get-context',
;; `study-set-uri' and `study-set-context' are mandatory.

;;; Code:

(require 'eieio)
(require 'seq)

;;; Objects

(defclass study-client ()
  ((reference :initarg :reference :read-only t)))

(eieio-declare-slots :reference)

;;; Generic

(cl-defgeneric study-open ((class (subclass study-client)) uri context)
  (if-let ((client (study--best-client class uri)))
      (prog1 client
        (study-set-uri client uri)
        (when context (study-set-context client context)))
    (study-new class uri context)))

(cl-defgeneric study-new ((class (subclass study-client)) uri context)
  (let ((supported (seq-filter (lambda (it) (study-supports it uri)) (eieio-class-children class))))
    (study-new (seq-first supported) uri context)))

(cl-defgeneric study-instances ((class (subclass study-client)))
  (seq-mapcat #'study-instances (eieio-class-children class)))

(cl-defgeneric study-supports ((class (subclass study-client)) uri)
  (seq-some #'identity (seq-map (lambda (it) (study-supports it uri)) (eieio-class-children class))))

(cl-defgeneric study-get-uri ((client study-client)))

(cl-defgeneric study-get-context ((client study-client)))

(cl-defgeneric study-set-uri ((client study-client) uri))

(cl-defgeneric study-set-context ((client study-client) context))

(cl-defgeneric study-next-context ((client study-client))
  (study-set-context client (1+ (study-get-page client))))

(cl-defgeneric study-previous-context ((client study-client))
  (study-set-context client (1- (study-get-context client))))

(defun study--best-client (class uri)
  (when-let ((all (study-instances class))
             (filter (lambda (it) (study-supports (eieio-object-class it) uri)))
             (priority (lambda (it) (string-distance (study-get-uri it) uri))))
    (seq-first (seq-sort-by priority #'< (seq-filter filter all)))))

;;; History

(cl-defstruct study-history
  (offset 0)
  (elements nil))

(defun study-history-go (hist n)
  (when-let* ((target (+ (study-history-offset hist) n))
              (length (seq-length (study-history-elements hist)))
              (clamped (min (max target 0) (1- length))))
    (setf (study-history-offset hist) clamped)))

(defun study-history-push (hist uri context)
  (let ((elem (cons uri context))
        (dropped (seq-drop (study-history-elements hist)
                           (study-history-offset hist))))
    (unless (equal elem (seq-first dropped))
      (setf (study-history-elements hist) (cons elem dropped))
      (setf (study-history-offset hist) 0))))

(defun study-history-uri (hist)
  (car (seq-elt (study-history-elements hist)
                (study-history-offset hist))))

(defun study-history-context (hist)
  (cdr (seq-elt (study-history-elements hist)
                (study-history-offset hist))))

(provide 'study-generic)

;;; study-generic.el ends here

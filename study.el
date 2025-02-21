;;; study.el --- Utilities for reasonably efficient studying  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Leo Gaskin

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

;; This package implements the user-facing parts of the study.el
;; system for operating external applications from within Emacs.

;;; Code:

(require 'eieio)
(require 'map)
(require 'study-generic)

(defvar study-current-client nil
  "Most active `study-client' object in the current context.")

(defvar study-history-map (make-hash-table :test #'equal)
  "Hash table of histories mapped to `study-client' objects.")

;;;###autoload
(defun study-find-file (filename &optional context)
  (interactive "f\nP")
  (when-let ((client (study-open 'study-client filename context)))
    (study-set-current-client client)
    (study-client-history-push client filename context)))

;;;###autoload
(defun study-find-file-other-window (filename &optional context)
  (interactive "f\nP")
  (when-let ((client (study-new 'study-client filename context)))
    (study-set-current-client client)
    (study-client-history-push client filename context)))

;;;###autoload
(defun study-next (&optional n)
  (interactive "p")
  (if (>= n 0)
      (study-next-context study-current-client n)
    (study-previous-context study-current-client (- n))))

;;;###autoload
(defun study-previous (&optional n)
  (interactive "p")
  (study-next (- n)))

;;;###autoload
(defun study-undo (n)
  (interactive "p")
  (when-let ((hist (study-client-history study-current-client)))
    (study-history-go hist n)
    (message "History: %s/%s"
             (1+ (study-history-offset hist))
             (seq-length (study-history-elements hist)))
    (study-set-uri study-current-client (study-history-uri hist))
    (study-set-context study-current-client (study-history-context hist))))

;;;###autoload
(defun study-redo (n)
  (interactive "p")
  (study-undo (- n)))

(defun study-set-current-client (client)
  (setq-default study-current-client client)
  (setq-local study-current-client client))

(defun study-client-history-push (client uri context)
  (let ((ref (oref client reference)))
    (unless (map-contains-key study-history-map ref)
      (map-put! study-history-map ref (make-study-history)))
    (study-history-push (map-elt study-history-map ref) uri context)))

(defun study-client-history (client)
  (let ((ref (oref client reference)))
    (map-elt study-history-map ref)))

(provide 'study)

;;; study.el ends here

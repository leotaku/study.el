;;; study.el --- Various utilities for students of the natural sciences -*- lexical-binding: t; -*-

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

(require 'study-generic)
(require 'study-okular)

(defvar study-current-client nil
  "Most active `study-client' object in the current context.")

(defvar study-history-map (make-hash-table :test #'equal)
  "Hash table of histories mapped to `study-client' objects.")

;;;###autoload
(defun study-find-file (filename)
  (interactive (list (find-file-read-args
                      "Find file: "
                      (confirm-nonexistent-file-or-buffer))))
  (study-open 'study-client (car filename)))

;;;###autoload
(defun study-undo (n)
  (interactive "p")
  (when-let ((hist (study-client-history study-current-client)))
    (study-history-go hist n)
    (message "History: %s/%s"
             (1+ (study-history-offset hist))
             (seq-length (study-history-elements hist)))
    (study-set-uri study-current-client (study-history-uri hist))
    (study-set-page study-current-client (study-history-page hist))))

;;;###autoload
(defun study-redo (n)
  (interactive "p")
  (study-undo (- n)))

(defun study-set-current-client (client)
  (setq-default study-current-client client)
  (setq-local study-current-client client))

(defun study-client-history-push (client uri page)
  (let ((ref (oref client :reference)))
    (unless (map-contains-key study-history-map ref)
      (map-put! study-history-map ref (make-study-history)))
    (study-history-push (map-elt study-history-map ref) uri page)))

(defun study-client-history (client)
  (let ((ref (oref client :reference)))
    (map-elt study-history-map ref)))

(provide 'study)

;;; study.el ends here

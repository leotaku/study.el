;;; study-dired.el --- Integration between study.el and dired  -*- lexical-binding: t; -*-

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

;; This package implements integrations between the study.el system
;; and the builtin dired file browser.

;;; Code:

(require 'study)
(require 'dired)

;;;###autoload
(defun study-dired (&optional arg)
  (interactive "P")
  (let* ((filename (dired-file-name-at-point))
         (page (and (numberp arg) arg)))
    (when-let ((client (if (equal arg '(4))
                           (study-new 'study-client filename nil)
                         (study-open 'study-client filename page))))
      (study-set-current-client client)
      (study-client-history-push client filename page))))

(defun study--advice-dired-find-file (fn)
  (if (study-supports 'study-client (dired-file-name-at-point))
      (study-dired)
    (funcall fn)))

(defun study--advice-dired-find-file-other-window (fn)
  (if (study-supports 'study-client (dired-file-name-at-point))
      (study-dired '(4))
    (funcall fn)))

(with-eval-after-load 'dired
  (advice-add 'dired-find-file :around #'study--advice-dired-find-file)
  (advice-add 'dired-find-file-other-window :around #'study--advice-dired-find-file-other-window))

(provide 'study-dired)

;;; study-dired.el ends here

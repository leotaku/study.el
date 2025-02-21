;;; study-dired.el --- Integration between study.el and dired  -*- lexical-binding: t; -*-

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

;; This package implements integrations between the study.el system
;; and the builtin dired file browser.

;;; Code:

(require 'study)
(require 'dired)

;;;###autoload
(defun study-dired (&optional arg)
  (interactive "P")
  (let ((filename (dired-file-name-at-point))
        (context (and (numberp arg) arg)))
    (if (equal arg '(4))
        (study-find-file-other-window filename context)
      (study-find-file filename context))))

(defun study--advice-dired-find-file (fn)
  (let ((filename (dired-file-name-at-point)))
    (if (study-supports 'study-client filename)
        (study-find-file filename current-prefix-arg)
      (funcall fn))))

(defun study--advice-dired-find-file-other-window (fn)
  (let ((filename (dired-file-name-at-point)))
    (if (study-supports 'study-client filename)
        (study-find-file-other-window filename current-prefix-arg)
      (funcall fn))))

(defun study--advice-dired-mouse-find-file (fn &rest args)
  (apply fn (car args) #'study-find-file (cddr args)))

(with-eval-after-load 'dired
  (advice-add 'dired-find-file :around #'study--advice-dired-find-file)
  (advice-add 'dired-find-file-other-window :around #'study--advice-dired-find-file-other-window)
  (advice-add 'dired-mouse-find-file :around #'study--advice-dired-mouse-find-file))

(provide 'study-dired)

;;; study-dired.el ends here

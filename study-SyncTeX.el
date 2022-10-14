;;; study-SyncTeX.el --- Integration between study.el and SyncTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 14 October 2022
;; Homepage: https://github.com/leotaku/study.el
;; Keywords: tex unix documents
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

(defun study-okular-SyncTeX-handler ()
  (let* ((file (TeX-command-expand "%o"))
         (context (TeX-command-expand "src:%n%b")))
    (study-open 'study-okular-client file context)))

(with-eval-after-load 'tex
  (add-to-list
   'TeX-view-program-list-builtin
   '("study-SyncTeX" study-okular-SyncTeX-handler)))

(provide 'study-SyncTeX)

;;; study-dired.el ends here

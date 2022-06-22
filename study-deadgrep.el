;;; study-deadgrep.el --- Integration between study.el and deadgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 7 November 2021
;; Homepage: https://github.com/leotaku/study.el
;; Keywords: multimedia unix documents
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (deadgrep "0.11"))

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
;; and the external deadgrep.el package with a particular focus on
;; searching through and opening PDF files.

;; In order to search through non-text files, the ripgrep-all "rga"
;; executable is required and assumed to be present on the system.

;; Technically, all files searchable by the ripgrep-all "rga" tool
;; that are also supported by a `study-client' subclass should be
;; supported without any additional effort.

;;; Code:

(require 'study)
(require 'deadgrep)

(defconst study--deadgrep-regexp
  (rx "\x1b[32m" (group (+ digit))
      "\x1b[0m" (or ":" "-") (optional "Page " (group (+ digit)) ": ")
      (group (* anything))))

(defun study--deadgrep-split-line (line)
  (save-match-data
    (string-match study--deadgrep-regexp line)
    (let ((content (match-string 3 line))
          (number (or (match-string 2 line) (match-string 1 line)))
          (filename (deadgrep--extract-regexp deadgrep--filename-regexp line)))
      (list
       filename
       (and number (string-to-number number))
       (deadgrep--propertize-hits content)))))

(defun study--deadgrep-visit-result (fn &rest args)
  (if (study-supports 'study-client (deadgrep--filename))
      (when-let* ((filename (deadgrep--filename))
                  (page (deadgrep--line-number))
                  (client (study-open 'study-client filename page)))
        (study-set-current-client client)
        (study-client-history-push client filename page))
    (apply fn args)))

(defun study--deadgrep-visit-result-other-window (fn &rest args)
  (if (study-supports 'study-client (deadgrep--filename))
      (when-let* ((filename (deadgrep--filename))
                  (page (deadgrep--line-number))
                  (client (study-new 'study-client filename page)))
        (study-set-current-client client)
        (study-client-history-push client filename page))
    (apply fn args)))

(with-eval-after-load 'deadgrep
  (setq deadgrep-executable "rga")
  (define-key deadgrep-mode-map (kbd "u") #'study-undo)
  (define-key deadgrep-mode-map (kbd "U") #'study-redo)
  (advice-add 'deadgrep--split-line :override #'study--deadgrep-split-line)
  (advice-add 'deadgrep-visit-result :around #'study--deadgrep-visit-result)
  (advice-add 'deadgrep-visit-result-other-window :around #'study--deadgrep-visit-result-other-window))

(provide 'study-deadgrep)

;;; study-deadgrep.el ends here

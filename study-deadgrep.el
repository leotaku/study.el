;;; study-deadgrep.el --- To be written.  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'deadgrep)
(require 'study)

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

(defun study--deadgrep-vr (fn &rest args)
  (if (and (study-call nil :accepts (deadgrep--filename)))
      (study-view (deadgrep--filename) (deadgrep--line-number) nil)
    (apply fn args)))

(defun study--deadgrep-vrow (fn &rest args)
  (if (and (study-call nil :accepts (deadgrep--filename)))
      (study-view (deadgrep--filename) (deadgrep--line-number) t)
    (apply fn args)))

(with-eval-after-load 'deadgrep
  (setq deadgrep-executable "rga")
  (define-key deadgrep-mode-map (kbd "u") #'study-undo)
  (define-key deadgrep-mode-map (kbd "U") #'study-redo)
  (advice-add 'deadgrep--split-line :override #'study--deadgrep-split-line)
  (advice-add 'deadgrep-visit-result :around #'study--deadgrep-vr)
  (advice-add 'deadgrep-visit-result-other-window :around #'study--deadgrep-vrow))

(provide 'study-deadgrep)

;;; study-deadgrep.el ends here

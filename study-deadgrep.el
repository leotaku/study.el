;;; study-deadgrep.el --- Integration between study.el and deadgrep -*- lexical-binding: t; -*-

;;; Commentary:
;;

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

;;; study-dired.el --- Integration between study.el and dired -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'study)
(require 'dired)

;;; Code:

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

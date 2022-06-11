;;; study.el --- Various utilities for students of the natural sciences -*- lexical-binding: t; -*-

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

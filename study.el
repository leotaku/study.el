;;; study.el --- Various utilities for students of the natural sciences -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/study.el
;; Keywords: outlines, bib, multimedia, documents
;; Package-Version: 1.1.0
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

;; To be written.

(require 'dbus)
(require 'dired)
(require 'gv)
(require 'map)
(require 'seq)
(eval-when-compile
  (require 'cl-lib))

;;; Code:

;;;; Variables

(defgroup study nil
  ""
  :prefix "study-"
  :group 'tools)

(defcustom study-hist-limit 50
  ""
  :type 'natnum)

(defvar study-hist-alist nil "")

(defvar study-latest-viewer nil "")

(cl-defstruct study-he
  ""
  (viewer nil :read-only t)
  (offset 0)
  (entries nil))

;;;###autoload
(defun study-dired (&optional arg)
  (interactive "P")
  (let* ((filename (dired-file-name-at-point))
         (page (and (numberp arg) arg)))
    (study-view filename page (equal arg '(4)))))

;;;###autoload
(defun study-view (filename &optional page force-new-instance)
  (let* ((filename (expand-file-name filename default-directory))
         (viewer (study--best (study-okular-controller :list) filename)))
    (when (or force-new-instance (not viewer))
      (setq viewer (study-okular-controller :start)))
    (study-go-to viewer filename page)))

;;;###autoload
(defun study-undo (n)
  (interactive "p")
  (when-let ((he (map-elt study-hist-alist study-latest-viewer))
             (args (seq-elt (study-he-entries he) (study-hist-undo he n))))
    (message "History: %s/%s" (1+ (study-he-offset he)) (length (study-he-entries he)))
    (study-go-to (study-he-viewer he) (car args) (cadr args) t)))

;;;###autoload
(defun study-redo (n)
  (interactive "p")
  (study-undo (- n)))

(defun study-okular-controller (action &optional id data)
  (cl-flet ((call (&rest args) (apply 'dbus-call-method :session id "/okular" "org.kde.okular" args))
            (filter (names) (seq-filter (lambda (it) (string-prefix-p "org.kde.okular" it)) names)))
    (pcase action
      (:accepts (and (stringp data) (string-equal (file-name-extension data) "pdf")))
      (:start (format "org.kde.okular-%s" (study--systemd-run "okular")))
      (:list (filter (dbus-list-names :session)))
      (:pid (string-to-number (string-trim-left id ".*-")))
      (:get-file (let ((name (call "currentDocument"))) (unless (file-directory-p name) name)))
      (:get-page (call "currentPage"))
      (:set-file (call "openDocument" :string data))
      (:set-page (call "goToPage" :uint32 data))
      (:next-page (call "slotNextPage"))
      (_ (error "Unknown method")))))

(defun study-call (viewer-id action &optional data)
  (study-okular-controller action viewer-id data))

(defun study-wrap-dbus (fn viewer-id &rest args)
  (let* ((fake-object `((:signal :session ,dbus-interface-dbus "NameOwnerChanged") nil))
         (callback (lambda (bus _ _) (apply fn bus args) (dbus-unregister-object fake-object))))
    (if (member viewer-id (dbus-list-names :session))
        (apply fn viewer-id args)
      (dbus-register-signal
       :session dbus-service-dbus dbus-path-dbus dbus-interface-dbus
       "NameOwnerChanged" callback))))

(defun study-go-to (viewer-id &optional filename page history-ignore)
  (let ((current-filename (study-call viewer-id :get-file))
        (current-page (study-call viewer-id :get-page))
        (something-happened nil))
    (let ((current-filename (and current-filename (expand-file-name current-filename)))
          (filename (and filename (expand-file-name filename))))
      (when (and filename (or (not current-filename) (not (file-equal-p filename current-filename))))
        (study-call viewer-id :set-file filename)
        (setq something-happened t))
      (when (and page (not (eq page current-page)))
        (study-call viewer-id :set-page page)
        (setq something-happened t))
      (when (and something-happened (not history-ignore))
        (setq study-latest-viewer viewer-id)
        (study-hist-push viewer-id current-filename current-page)
        (study-hist-push viewer-id filename page)))))

(advice-add 'study-go-to :around #'study-wrap-dbus)

;;; History logic

(defun study-hist-push (viewer-id &optional file page)
  (when (and file page)
    (let* ((he (map-elt study-hist-alist viewer-id))
           (it (list file page)))
      (when (not he)
        (setf (map-elt study-hist-alist viewer-id) (make-study-he :viewer viewer-id))
        (setq he (map-elt study-hist-alist viewer-id)))
      (setf (study-he-entries he) (seq-drop (study-he-entries he) (study-he-offset he)))
      (unless (or (equal it (seq-first (study-he-entries he))))
        (push it (study-he-entries he)))
      (setf (study-he-entries he) (seq-uniq (seq-take (study-he-entries he) study-hist-limit)))
      (setf (study-he-offset he) 0))))

(cl-defun study-hist-undo (he &optional (n 1))
  (setf (study-he-offset he)
        (min (1- (length (study-he-entries he)))
             (max (+ (study-he-offset he) n) 0))))

;;; Sort logic

(defun study--call-process-to-string (command &rest args)
  (with-output-to-string
    (apply #'call-process command nil standard-output nil args)))

(defun study--systemd-run (command)
  (let* ((unit (format "%s-%s" command (random)))
         (_ (study--call-process-to-string "systemd-run" "--user" "--unit" unit command))
         (output (study--call-process-to-string "systemctl" "--user" "show" "--property" "MainPID" "--value" unit)))
    (string-to-number (string-trim output))))

(defun study--best (viewers filename)
  (seq-first (study--sort-by-desktop (study--sort-by-filename viewers filename))))

(defun study--sort-by-filename (viewers filename)
  (seq-sort-by
   (lambda (it) (study--string-distance filename (study-call it :get-file)))
   #'< viewers))

(defun study--string-distance (string1 string2)
  (if (or (seq-empty-p string1) (seq-empty-p string2))
      0.5
    (string-distance string1 string2)))

(defun study--sort-by-desktop (viewers)
  (if-let ((_ (fboundp 'x-window-property))
           (pids (study--desktop-pids)))
      (seq-sort-by
       (lambda (it) (if (memq (study-call it :pid) pids) 1 0))
       #'> viewers)
    (prog1 viewers)))

(defun study--desktop-pids ()
  (seq-map
   (lambda (it) (and (= (x-window-property "_NET_WM_DESKTOP" nil "CARDINAL" it nil t)
                        (x-window-property "_NET_CURRENT_DESKTOP" nil "CARDINAL" 0 nil t))
                     (x-window-property "_NET_WM_PID" nil "CARDINAL" it nil t)))
   (x-window-property "_NET_CLIENT_LIST" nil "WINDOW" 0 nil t)))

;;; Integrations

(with-eval-after-load 'deadgrep
  (require 'study-deadgrep))

(provide 'study)

;;; study.el ends here

;;; study-okular.el --- Integration between study.el and the Okular PDF viewer  -*- lexical-binding: t; -*-

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

;; This package implements a bridge between Emacs and the Okular PDF
;; viewer as part of the study.el system.

;; For communication, both systemd and dbus user sessions are used and
;; thus required to be supported by the system.

;;; Code:

(require 'eieio)
(require 'seq)
(require 'url)
(require 'dbus)
(require 'study-generic)

(defclass study-okular-client (study-client)
  ((active :initarg :active :initform nil)))

(eieio-declare-slots :reference :active)

(cl-defmethod study-new ((class (subclass study-okular-client)) uri context)
  (let ((ref (format
              "org.kde.okular-%s"
              (study--systemd-run
               "okular" "--noraise"
               (format "%s#%s" (expand-file-name uri) (or context ""))))))
    (make-instance class :reference ref)))

(cl-defmethod study-supports ((_class (subclass study-okular-client)) uri)
  (when-let ((parsed (url-generic-parse-url uri)))
    (and (seq-contains-p '("http" "https" "file" nil) (url-type parsed))
         (string-suffix-p "pdf" (seq-first (url-path-and-query parsed))))))

(cl-defmethod study-instances ((class (subclass study-okular-client)))
  (seq-map (lambda (it)
             (make-instance class
                            :reference it
                            :active t))
           (seq-filter
            (lambda (it) (string-prefix-p "org.kde.okular" it))
            (dbus-list-names :session))))

(cl-defmethod study-get-uri ((client study-okular-client))
  (study--okular-sync-dbus client "currentDocument"))

(cl-defmethod study-get-context ((client study-okular-client))
  (study--okular-sync-dbus client "currentPage"))

(cl-defmethod study-set-uri ((client study-okular-client) uri)
  (let ((normalized (expand-file-name uri)))
    (unless (equal normalized (study-get-uri client))
      (study--okular-async-dbus client "openDocument" :string normalized))))

(cl-defmethod study-set-context ((client study-okular-client) context)
  (cond
   ((numberp context)
    (when (not (equal context (study-get-context client)))
      (study--okular-async-dbus client "goToPage" :uint32 context)))
   ((stringp context)
    (let ((uri (concat "file://" (study-get-uri client) "#" context)))
      (study--okular-async-dbus client "openDocument" :string uri)))))

(cl-defmethod study-next-context ((client study-okular-client) &optional n)
  (study-set-context client (+ (study-get-context client) n)))

(cl-defmethod study-previous-context ((client study-okular-client) &optional n)
  (study-set-context client (- (study-get-context client) n)))

(cl-defmethod study--okular-sync-dbus ((client study-okular-client) method &rest args)
  (let ((ref (oref client :reference)))
    (apply 'dbus-call-method :session ref "/okular" "org.kde.okular" method args)))

(cl-defmethod study--okular-async-dbus ((client study-okular-client) method &rest args)
  (let ((ref (oref client :reference)))
    (if (oref client :active)
        (apply 'dbus-call-method :session ref "/okular" "org.kde.okular" method args)
      (study--with-dbus
       (lambda (_)
         (oset client :active t)
         (apply
          'dbus-call-method-asynchronously
          :session ref "/okular" "org.kde.okular" method nil args))
       ref))))

(defun study--with-dbus (fn ref &rest args)
  (let* ((fake-object `((:signal :session ,dbus-interface-dbus "NameOwnerChanged") nil))
         (callback (lambda (bus _ _) (apply fn bus args) (dbus-unregister-object fake-object))))
    (if (member ref (dbus-list-names :session))
        (apply fn ref args)
      (dbus-register-signal
       :session dbus-service-dbus dbus-path-dbus dbus-interface-dbus
       "NameOwnerChanged" callback))))

(defun study--systemd-run (command &rest args)
  (let* ((unit (format "%s-%s" command (random)))
         (_ (apply #'study--call-process-to-string "systemd-run" "--user" "--unit" unit command args))
         (output (study--call-process-to-string "systemctl" "--user" "show" "--property" "MainPID" "--value" unit)))
    (string-to-number (string-trim output))))

(defun study--call-process-to-string (command &rest args)
  (with-output-to-string
    (apply #'call-process command nil standard-output nil args)))

(provide 'study-okular)

;;; study-okular.el ends here

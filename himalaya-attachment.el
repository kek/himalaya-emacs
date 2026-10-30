;;; himalaya-attachment.el --- Attachment management of email client Himalaya CLI  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo
;; Copyright (C) 2022-2026 soywod <clement.douin@posteo.net>

;; Author: Dante Catalfamo
;;      soywod <clement.douin@posteo.net>
;; Maintainer: soywod <clement.douin@posteo.net>
;;      Dante Catalfamo
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/dantecatalfamo/himalaya-emacs
;; Keywords: mail comm

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Interface for the email client Himalaya CLI
;; <https://github.com/soywod/himalaya>

;;; Code:

(require 'himalaya-process)
(require 'himalaya-account)
(require 'himalaya-mailbox)
(require 'himalaya-envelope-mark)

(defcustom himalaya-attachment-download-dir nil
  "Directory where attachments are downloaded. When nil, Himalaya CLI
uses the `downloads-dir' of its configuration."
  :type '(choice (const :tag "From Himalaya CLI configuration" nil) directory)
  :group 'himalaya)

(defun himalaya--download-attachments (ids callback &optional paths)
  "Download attachment(s) of message(s) matching envelope IDS, one
message at a time, then call CALLBACK with the downloaded file paths.
PATHS accumulates the paths of the messages already downloaded."
  (let ((ids (if (listp ids) ids (list ids))))
    (if (null ids)
        (funcall callback (reverse paths))
      (message "Downloading attachments of message %s…" (car ids))
      (himalaya--run
       (lambda (output)
         (himalaya--download-attachments
          (cdr ids)
          callback
          (append (reverse (mapcar (lambda (attachment) (plist-get attachment :path))
                                   (plist-get output :attachments)))
                  paths)))
       nil
       "attachment"
       "download"
       (when himalaya-account (list "--account" himalaya-account))
       (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
       (when himalaya-attachment-download-dir
         (list "--dir" (expand-file-name himalaya-attachment-download-dir)))
       (format "%s" (car ids))))))

(defun himalaya--report-downloads (paths)
  "Report the downloaded attachment PATHS."
  (if paths
      (message "Downloaded %d attachment(s) to %s"
               (length paths)
               (string-join (delete-dups (mapcar #'file-name-directory paths)) ", "))
    (message "No attachment to download")))

(defun himalaya-download-marked-attachments ()
  "Download attachment(s) of message(s) matching marked envelope(s),
or matching the envelope at point if mark is not set."
  (interactive)
  (himalaya--download-attachments
   (or himalaya-marked-ids (list (tabulated-list-get-id)))
   (lambda (paths)
     (himalaya--report-downloads paths)
     (himalaya-unmark-all-envelopes t))))

(defun himalaya-download-current-attachments ()
  "Download attachment(s) of message matching the current envelope."
  (interactive)
  (himalaya--download-attachments himalaya-id #'himalaya--report-downloads))

(provide 'himalaya-attachment)
;;; himalaya-attachment.el ends here

;;; himalaya-mailbox.el --- Mailbox management for email client Himalaya CLI  -*- lexical-binding: t -*-

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

(define-obsolete-variable-alias 'himalaya-folder 'himalaya-mailbox "2.0")

(defvar himalaya-mailbox nil
  "The current mailbox. When nil, Himalaya CLI uses the inbox.")

(defun himalaya--list-mailboxes (callback)
  "Fetch all mailboxes of the current account."
  (message "Listing mailboxes…")
  (himalaya--run
   (lambda (output) (funcall callback (plist-get output :mailboxes)))
   nil
   "mailbox"
   "list"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--pick-mailbox (prompt callback)
  "Ask user to pick a mailbox using PROMPT then call CALLBACK with
the identifier of the selected mailbox. Input not matching any
mailbox is passed as is, so aliases like \"trash\" can be typed."
  (himalaya--list-mailboxes
   (lambda (mailboxes)
     (let* ((ids (mapcar (lambda (mailbox)
                           (cons (plist-get mailbox :name) (plist-get mailbox :id)))
                         mailboxes))
            (name (completing-read prompt ids)))
       (funcall callback (or (cdr (assoc name ids)) name))))))

(defun himalaya--expunge-mailbox (callback)
  "Expunge the current mailbox of the current account. Expunging is
IMAP-specific since Himalaya CLI v2."
  (let ((mailbox (or himalaya-mailbox "INBOX")))
    (message "Expunging mailbox %s…" mailbox)
    (himalaya--run-plain
     callback
     nil
     "imap"
     "expunge"
     (when himalaya-account (list "--account" himalaya-account))
     mailbox)))

(defun himalaya-switch-mailbox ()
  "Ask user to pick a mailbox, set it as the current mailbox then
list envelopes."
  (interactive)
  (himalaya--pick-mailbox
   "Mailbox: "
   (lambda (mailbox)
     (setq himalaya-mailbox mailbox)
     (setq himalaya-page 1)
     (himalaya--update-mode-line)
     (revert-buffer))))

(defun himalaya-expunge-mailbox ()
  "Expunge the current mailbox (IMAP only), permanently removing
messages flagged as deleted, then list envelopes."
  (interactive)
  (when (y-or-n-p (format "Expunge mailbox %s? " (or himalaya-mailbox "INBOX")))
    (himalaya--expunge-mailbox
     (lambda (output)
       (message "%s" (string-trim output))
       (revert-buffer)))))

(define-obsolete-function-alias 'himalaya-switch-folder #'himalaya-switch-mailbox "2.0")
(define-obsolete-function-alias 'himalaya-expunge-folder #'himalaya-expunge-mailbox "2.0")

(provide 'himalaya-mailbox)
;;; himalaya-mailbox.el ends here

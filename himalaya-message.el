;;; himalaya-message.el --- Message management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(require 'subr-x)
(require 'seq)
(require 'mailheader)
(require 'message)
(require 'mm-decode)
(require 'mm-view)
(require 'rfc2047)
(require 'himalaya-process)
(require 'himalaya-account)
(require 'himalaya-mailbox)
(require 'himalaya-envelope-mark)
(require 'himalaya-flag)
(require 'himalaya-attachment)
(require 'himalaya-template)

(defcustom himalaya-html-browse-function #'eww-open-file
  "Function to use for browsing HTML emails.
This should be a function that takes a file path as an argument.
Examples: 'eww-open-file', 'xwidget-webkit-browse-url', 'browse-url-of-file'."
  :type 'function
  :group 'himalaya)

(defcustom himalaya-message-headers '("From" "To" "Cc" "Date" "Subject")
  "Headers to display when reading a message, in order."
  :type '(repeat string)
  :group 'himalaya)

(defvar-local himalaya--mime-handles nil
  "MIME handles of the message displayed in the current buffer.")

(defun himalaya--extract-headers (msg)
  "Extract MESSAGE headers."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--generate-write-buffer (buffer-name tpl)
  "Setup to be used to write a message."
  (switch-to-buffer (generate-new-buffer buffer-name))
  (insert (plist-get tpl :content))
  (goto-line (plist-get (plist-get tpl :cursor) :row))
  (move-to-column (plist-get (plist-get tpl :cursor) :col))
  (himalaya-message-write-mode)
  (set-buffer-modified-p nil))

(defun himalaya--read-message (id preview callback)
  "Fetch the raw RFC 5322 message matching the envelope ID from the
current mailbox on the current account, then call CALLBACK with it as
a unibyte string. Unless PREVIEW is non-nil, the message is flagged as
seen."
  (message "Reading message %s…" id)
  (himalaya--run-raw
   (lambda (msg) (funcall callback (replace-regexp-in-string "\r$" "" msg)))
   nil
   "message"
   "read"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
   "--raw"
   (unless preview "--seen")
   (format "%s" id))) ; force id as a string

(defun himalaya--decode-header (value)
  "Decode the raw header VALUE, RFC 2047 encoded words included."
  (when value
    (rfc2047-decode-string (decode-coding-string value 'utf-8))))

(defun himalaya--dissect-message (raw)
  "Dissect the RAW message. Return a cons of the decoded headers to
display, as an alist, and the MIME handle of the message."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert raw)
    (goto-char (point-min))
    (cons (save-restriction
            (mail-narrow-to-head)
            (mapcar (lambda (name) (cons name (himalaya--decode-header (mail-fetch-field name))))
                    himalaya-message-headers))
          (mm-dissect-buffer t t))))

(defun himalaya--insert-part (handle)
  "Insert the MIME part HANDLE at point. Parts Emacs can display are
displayed inline, other parts are inserted as buttons saving them."
  (cond
   ((stringp (car handle))
    (let ((parts (cdr handle)))
      (pcase (car handle)
        ("multipart/alternative"
         (himalaya--insert-part (or (mm-preferred-alternative parts) (car parts))))
        ;; the first part is the root, the others being what it refers to
        ("multipart/related" (himalaya--insert-part (car parts)))
        (_ (mapc #'himalaya--insert-part parts)))))
   ((and (not (equal (car (mm-handle-disposition handle)) "attachment"))
         (mm-inlinable-p handle)
         (mm-inlined-p handle))
    (unless (bolp) (insert "\n"))
    (save-restriction
      (narrow-to-region (point) (point))
      (mm-display-inline handle)
      (goto-char (point-max))))
   (t
    (unless (bolp) (insert "\n"))
    (insert-text-button
     (format "[%s, %s]" (or (mm-handle-filename handle) "attachment") (mm-handle-media-type handle))
     'action (lambda (_) (mm-save-part handle))
     'help-echo "Save this part")
    (insert "\n"))))

(defun himalaya--find-part (handle type)
  "Find the first part of MIME TYPE in HANDLE."
  (if (stringp (car handle))
      (seq-some (lambda (part) (himalaya--find-part part type)) (cdr handle))
    (when (equal (mm-handle-media-type handle) type) handle)))

(defun himalaya--destroy-mime-handles ()
  "Free the MIME handles of the current buffer."
  (when himalaya--mime-handles
    (mm-destroy-parts himalaya--mime-handles)
    (setq himalaya--mime-handles nil)))

(defun himalaya--copy-messages (ids folder callback)
  "Copy message(s) matching envelope IDS from current folder of
current account to target FOLDER."
  (message "Copying message(s) to %s…" folder)
  (himalaya--run
   callback
   nil
   "message"
   "copy"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--folder" himalaya-mailbox))
   folder
   ids))

(defun himalaya--move-messages (ids folder callback)
  "Move message(s) matching envelope IDS from current folder of
current account to target FOLDER."
  (message "Moving message(s) to %s…" folder)
  (himalaya--run
   callback
   nil
   "message"
   "move"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--folder" himalaya-mailbox))
   folder
   ids))

(defun himalaya--delete-messages (ids callback)
  "Delete message(s) matching envelope IDS from current folder of
current account."
  (message "Deleting message(s)…")
  (himalaya--run
   callback
   nil
   "message"
   "delete"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--folder" himalaya-mailbox))
   ids))

(defun himalaya--read-current-message (&optional preview pre-hook)
  "Read message matching current envelope id in current mailbox from
current account."
  (himalaya--read-message
   himalaya-id
   preview
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((dissected (himalaya--dissect-message msg))
	    (headers (car dissected))
	    (subject (or (cdr (assoc "Subject" headers)) "")))
       (switch-to-buffer (format "*%s*" subject))
       (let ((inhibit-read-only t))
         (himalaya-read-message-mode)
         (erase-buffer)
         (setq himalaya--mime-handles (cdr dissected))
         (dolist (header headers)
           (when (cdr header)
             (insert (propertize (concat (car header) ":") 'face himalaya-headers-face)
                     " " (cdr header) "\n")))
         (insert "\n")
         (himalaya--insert-part himalaya--mime-handles))
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (setq himalaya-subject subject)))))

(defun himalaya--read-current-message-raw (&optional pre-hook)
  "Read raw message matching current envelope id in current mailbox
from current account."
  (himalaya--read-message
   himalaya-id
   t
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((msg (decode-coding-string msg 'utf-8))
            (headers (himalaya--extract-headers msg))
	    (subject (himalaya--decode-header (alist-get 'subject headers))))
       (switch-to-buffer (format "*Raw: %s*" subject))
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (insert msg)
	 (set-buffer-modified-p nil)
	 (himalaya-read-message-raw-mode)
	 (goto-char (point-min))
	 (setq himalaya-subject subject))))))

(defun himalaya-read-current-message-plain (&optional preview)
  "Read message matching current envelope id in current folder from
current account. If called with \\[universal-argument], enable
PREVIEW, which means that the flag Seen will not be applied to its
envelope."
  (interactive "P")
  (himalaya--read-current-message preview #'kill-current-buffer))

(defun himalaya-read-current-message-raw ()
  "Read raw message matching current envelope id in current mailbox
from current account."
  (interactive)
  (himalaya--read-current-message-raw #'kill-current-buffer))

(defun himalaya--browse-html (handle)
  "Browse the first HTML part of HANDLE using
`himalaya-html-browse-function'."
  (let* ((part (or (himalaya--find-part handle "text/html")
                   (user-error "No HTML part in this message")))
         (charset (or (mail-content-type-get (mm-handle-type part) 'charset) "utf-8"))
         (html (mm-decode-string (mm-get-part part) (intern (downcase charset))))
         (file (make-temp-file "himalaya-" nil ".html"))
         (coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert "<meta charset=\"utf-8\">\n" html))
    (funcall himalaya-html-browse-function file)))

(defun himalaya-read-current-message-html ()
  "Read the HTML part of the current message using
`himalaya-html-browse-function'."
  (interactive)
  (if himalaya--mime-handles
      (himalaya--browse-html himalaya--mime-handles)
    (himalaya--read-message
     himalaya-id
     t
     (lambda (msg)
       (let ((handle (cdr (himalaya--dissect-message msg))))
         (unwind-protect (himalaya--browse-html handle)
           (mm-destroy-parts handle)))))))

(defun himalaya-reply-to-current-message (&optional reply-all)
  "Open a new buffer with a reply template to the current email.
If called with \\[universal-argument], email will be REPLY-ALL."
  (interactive "P")
  (himalaya--reply-template
   himalaya-id
   (lambda (tpl)
     (setq himalaya-reply t)
     (himalaya--generate-write-buffer (format "*Reply: %s*" himalaya-subject) tpl))
   reply-all))

(defun himalaya-forward-current-message ()
  "Open a new buffer with a forward template to the current email."
  (interactive)
  (himalaya--forward-template
   himalaya-id
   (lambda (tpl)
     (setq himalaya-reply nil)
     (himalaya--generate-write-buffer (format "*Forward: %s*" himalaya-subject) tpl))))

(defun himalaya--adjacent-envelope-id (id n)
  "Return the id of the envelope N lines away from the envelope ID in
the envelope list, or nil if there is none."
  (when-let* ((buffer (get-buffer "*Himalaya Envelopes*")))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
          (forward-line 1))
        (when (and (not (eobp)) (zerop (forward-line n)) (not (eobp)))
          (tabulated-list-get-id))))))

(defun himalaya-next-message ()
  "Go to the message of the next envelope in the envelope list."
  (interactive)
  (setq himalaya-id (or (himalaya--adjacent-envelope-id himalaya-id 1)
                        (user-error "At end of envelope list")))
  (himalaya--read-current-message nil #'kill-current-buffer))

(defun himalaya-prev-message ()
  "Go to the message of the previous envelope in the envelope list."
  (interactive)
  (setq himalaya-id (or (himalaya--adjacent-envelope-id himalaya-id -1)
                        (user-error "At beginning of envelope list")))
  (himalaya--read-current-message nil #'kill-current-buffer))

(defun himalaya-read-message-at-point (&optional preview)
  "Pick the envelope at point and read its associated message. If
called with \\[universal-argument], enable PREVIEW, which means that
the flag Seen will not be applied to its envelope."
  (interactive "P")
  (setq himalaya-id (tabulated-list-get-id))
  (himalaya--read-current-message preview))

(defun himalaya-write-new-message ()
  "Compose a new message in a buffer."
  (interactive)
  (himalaya--write-template
   (lambda (tpl)
     (setq himalaya-reply nil)
     (himalaya--generate-write-buffer "*Himalaya New Message*" tpl))))

(defun himalaya-reply-to-message-at-point (&optional reply-all)
  "Pick the envelope at point then reply to its associated message.
If called with \\[universal-argument], message will be REPLY-ALL."
  (interactive "P")
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-reply reply-all)))

(defun himalaya-forward-message-at-point ()
  "Pick the envelope at point then forward its associated message."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-forward)))

(defun himalaya-copy-marked-messages ()
  "Copy message(s) matching marked envelope(s) (or envelope at point)
from current folder of current account to selected folder."
  (interactive)
  (himalaya--pick-mailbox
   "Copy to folder: "
   (lambda (folder)
     (himalaya--copy-messages
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      folder
      (lambda (status)
	(message "%s" (string-trim status))
	(himalaya-unmark-all-envelopes t))))))

(defun himalaya-move-marked-messages ()
  "Move message(s) matching marked envelope(s) (or envelope at point)
from current folder of current account to selected folder."
  (interactive)
  (himalaya--pick-mailbox
   "Move to folder: "
   (lambda (folder)
     (let ((prev-point (point))
	   (ids (or himalaya-marked-ids (list (tabulated-list-get-id)))))
       (himalaya--move-messages
	ids
	folder
	(lambda (status)
	  (message "%s" (string-trim status))
	  (himalaya-unmark-all-envelopes t)
	  (revert-buffer)
	  (goto-char prev-point)))))))

(defun himalaya-delete-marked-messages ()
  "Delete message(s) matching marked envelope(s) (or envelope at
point) from current folder of current account."
  (interactive)
  (let* ((prev-point (point))
	 (envelope (tabulated-list-get-entry))
         (subject (substring-no-properties (elt envelope 2)))
	 (subject-or-ids (if himalaya-marked-ids (string-join himalaya-marked-ids ", ") subject)))
    (when (y-or-n-p (format "Delete message(s) %s? " subject-or-ids))
      (himalaya--delete-messages
       (or himalaya-marked-ids (tabulated-list-get-id))
       (lambda (status)
	 (message "%s" (string-trim status))
	 (himalaya-unmark-all-envelopes t)
	 (revert-buffer)
	 (goto-char prev-point))))))

(defun himalaya-send-buffer ()
  "Send the current buffer."
  (interactive)
  (himalaya--send-template
   (buffer-string)
   (lambda (status)
     (if himalaya-reply
	 (himalaya--add-flag
	  himalaya-id
	  "Answered"
	  (lambda (_)
	    (message "%s" (string-trim status))
	    (set-buffer-modified-p nil)
	    (kill-current-buffer)
	    (himalaya-list-envelopes)))
       (message "%s" (string-trim status))
       (set-buffer-modified-p nil)
       (kill-current-buffer)))))

(defvar himalaya-read-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-download-current-attachments)
    (define-key map (kbd "R") #'himalaya-read-current-message-raw)
    (define-key map (kbd "r") #'himalaya-reply-to-current-message)
    (define-key map (kbd "f") #'himalaya-forward-current-message)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-message)
    (define-key map (kbd "p") #'himalaya-prev-message)
    (define-key map (kbd "h") #'himalaya-read-current-message-html)
    map))

(define-derived-mode himalaya-read-message-mode special-mode "Himalaya-Read"
  "Message reading mode."
  (add-hook 'kill-buffer-hook #'himalaya--destroy-mime-handles nil t)
  (add-hook 'change-major-mode-hook #'himalaya--destroy-mime-handles nil t))

(defvar himalaya-read-message-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-download-current-attachments)
    (define-key map (kbd "R") #'himalaya-read-current-message-plain)
    (define-key map (kbd "r") #'himalaya-reply-to-current-message)
    (define-key map (kbd "f") #'himalaya-forward-current-message)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-message)
    (define-key map (kbd "p") #'himalaya-prev-message)
    (define-key map (kbd "h") #'himalaya-read-current-message-html)
    map))

(define-derived-mode himalaya-read-message-raw-mode message-mode "Himalaya-Read-Raw"
  "Himalaya raw message reading mode."
  (setq mail-header-separator ""))

(defvar himalaya-message-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'himalaya-send-buffer)
    map))

(define-derived-mode himalaya-message-write-mode message-mode "Himalaya-Write"
  "Himalaya message writing mode."
  (setq mail-header-separator ""))

(provide 'himalaya-message)
;;; himalaya-message.el ends here

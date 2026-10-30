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
(require 'mml)
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

(defvar-local himalaya--compose-account nil
  "Account the message of the current buffer is written from.")

(defvar-local himalaya--compose-reply-to nil
  "Mailbox and id of the message the current buffer replies to, as a
cons, or nil if it is not a reply.")

(defvar-local himalaya--compose-temp-dirs nil
  "Temporary directories holding the parts the current buffer attaches.")

(defvar himalaya--template-temp-dirs nil
  "Temporary directories created while converting a template.")

(defconst himalaya--template-skipped-headers
  '("Date" "MIME-Version" "Content-Type" "Content-Transfer-Encoding" "Content-Disposition")
  "Template headers not shown in the message being written, regenerated
when sending.")

(defun himalaya--extract-headers (msg)
  "Extract MESSAGE headers."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--template-headers ()
  "Return the decoded headers of the current buffer, narrowed to the
headers, as a string, without the MIME headers."
  (let (lines)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^: \t\n]+\\):[ \t]*" nil t)
      (let ((name (match-string 1))
            (start (point)))
        (forward-line 1)
        (while (looking-at "[ \t]") (forward-line 1))
        (unless (member-ignore-case name himalaya--template-skipped-headers)
          (push (format "%s: %s" name
                        (himalaya--decode-header
                         (replace-regexp-in-string
                          "\n[ \t]+" " " (string-trim-right (buffer-substring start (point))))))
                lines))))
    (dolist (name '("To" "Subject"))
      (unless (seq-some (lambda (line) (string-prefix-p (concat name ":") line t)) lines)
        (push (concat name ": ") lines)))
    (concat (string-join (nreverse lines) "\n") "\n")))

(defun himalaya--part-to-mml (handle)
  "Return the MIME part HANDLE as MML. Text bodies are decoded, other
parts are saved to a temporary directory, added to
`himalaya--template-temp-dirs', and referenced by MML tags."
  (cond
   ((stringp (car handle))
    (if (equal (car handle) "multipart/alternative")
        (himalaya--part-to-mml (or (himalaya--find-part handle "text/plain") (cadr handle)))
      (mapconcat #'himalaya--part-to-mml (cdr handle) "")))
   ((and (equal (mm-handle-media-type handle) "text/plain")
         (not (equal (car (mm-handle-disposition handle)) "attachment")))
    (let ((charset (or (mail-content-type-get (mm-handle-type handle) 'charset) "utf-8")))
      (with-temp-buffer
        (insert (mm-decode-string (mm-get-part handle) (intern (downcase charset))))
        (mml-quote-region (point-min) (point-max))
        (buffer-string))))
   (t
    (let* ((dir (make-temp-file "himalaya-" t))
           (file (expand-file-name
                  (file-name-nondirectory (or (mm-handle-filename handle) "attachment"))
                  dir)))
      (push dir himalaya--template-temp-dirs)
      (mm-save-part-to-file handle file)
      (with-temp-buffer
        (mml-insert-empty-tag 'part
                              'type (mm-handle-media-type handle)
                              'filename file
                              'disposition "attachment")
        (buffer-string))))))

(defun himalaya--strip-cr (raw)
  "Turn the CRLF line endings of the RAW message into LF."
  (replace-regexp-in-string "\r$" "" raw))

(defun himalaya--template-to-mml (raw)
  "Convert the raw RFC 5322 template RAW to the content of a buffer to
write a message in: decoded headers, a blank line then the body as
MML."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (himalaya--strip-cr raw))
    (goto-char (point-min))
    (let ((handle (mm-dissect-buffer t t)))
      (unwind-protect
          (concat (save-restriction
                    (mail-narrow-to-head)
                    (himalaya--template-headers))
                  "\n"
                  (himalaya--part-to-mml handle))
        (mm-destroy-parts handle)))))

(defun himalaya--generate-write-buffer (buffer-name tpl &optional reply-to)
  "Setup a buffer named BUFFER-NAME to write a message from the raw
template TPL. REPLY-TO is the mailbox and id of the message replied
to, as a cons."
  (let* ((himalaya--template-temp-dirs nil)
         (content (himalaya--template-to-mml tpl)))
    (switch-to-buffer (generate-new-buffer buffer-name))
    (insert content)
    (himalaya-message-write-mode)
    (setq himalaya--compose-account himalaya-account)
    (setq himalaya--compose-reply-to reply-to)
    (setq himalaya--compose-temp-dirs himalaya--template-temp-dirs)
    (goto-char (point-min))
    (if (re-search-forward "^To:[ \t]*$" (save-excursion (message-goto-body) (point)) t)
        (goto-char (match-end 0))
      (message-goto-body))
    (set-buffer-modified-p nil)))

(defun himalaya--delete-compose-temp-dirs ()
  "Delete the temporary directories of the current buffer."
  (dolist (dir himalaya--compose-temp-dirs)
    (delete-directory dir t))
  (setq himalaya--compose-temp-dirs nil))

(defun himalaya--encode-buffer ()
  "Return the message of the current buffer as raw RFC 5322 bytes,
its MML turned into MIME."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (mail-header-separator ""))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (save-restriction
        (message-narrow-to-headers-or-head)
        (flush-lines "^[^: \t\n]+:[ \t]*$")
        (unless (mail-fetch-field "Date")
          (goto-char (point-max))
          (insert "Date: " (message-make-date) "\n")))
      (mml-to-mime)
      (encode-coding-string (buffer-string) 'raw-text-unix))))

(defun himalaya--read-message (id preview callback)
  "Fetch the raw RFC 5322 message matching the envelope ID from the
current mailbox on the current account, then call CALLBACK with it as
a unibyte string. Unless PREVIEW is non-nil, the message is flagged as
seen."
  (message "Reading message %s…" id)
  (himalaya--run-raw
   (lambda (msg) (funcall callback (himalaya--strip-cr msg)))
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

(defun himalaya--copy-messages (ids mailbox callback)
  "Copy message(s) matching envelope IDS from current mailbox of
current account to target MAILBOX."
  (message "Copying message(s) to %s…" mailbox)
  (himalaya--run
   callback
   nil
   "message"
   "copy"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--from" himalaya-mailbox))
   "--to" mailbox
   ids))

(defun himalaya--move-messages (ids mailbox callback)
  "Move message(s) matching envelope IDS from current mailbox of
current account to target MAILBOX."
  (message "Moving message(s) to %s…" mailbox)
  (himalaya--run
   callback
   nil
   "message"
   "move"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--from" himalaya-mailbox))
   "--to" mailbox
   ids))

(defun himalaya--delete-messages (ids callback)
  "Delete message(s) matching envelope IDS from current mailbox of
current account. Himalaya CLI moves them to the trash, or removes them
for good when they already are in the trash."
  (message "Deleting message(s)…")
  (himalaya--run
   callback
   nil
   "message"
   "delete"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
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
  (let ((id himalaya-id)
        (mailbox himalaya-mailbox))
    (himalaya--reply-template
     id
     (lambda (tpl)
       (himalaya--generate-write-buffer (format "*Reply: %s*" himalaya-subject) tpl (cons mailbox id)))
     reply-all)))

(defun himalaya-forward-current-message ()
  "Open a new buffer with a forward template to the current email."
  (interactive)
  (himalaya--forward-template
   himalaya-id
   (lambda (tpl)
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
     (himalaya--generate-write-buffer "*Himalaya New Message*" tpl))))

(defun himalaya-reply-to-message-at-point (&optional reply-all)
  "Pick the envelope at point then reply to its associated message.
If called with \\[universal-argument], message will be REPLY-ALL."
  (interactive "P")
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-reply-to-current-message reply-all)))

(defun himalaya-forward-message-at-point ()
  "Pick the envelope at point then forward its associated message."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-forward-current-message)))

(defun himalaya-copy-marked-messages ()
  "Copy message(s) matching marked envelope(s) (or envelope at point)
from current mailbox of current account to selected mailbox."
  (interactive)
  (himalaya--pick-mailbox
   "Copy to mailbox: "
   (lambda (mailbox)
     (himalaya--copy-messages
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      mailbox
      (lambda (output)
	(message "%s" (plist-get output :message))
	(himalaya-unmark-all-envelopes t))))))

(defun himalaya-move-marked-messages ()
  "Move message(s) matching marked envelope(s) (or envelope at point)
from current mailbox of current account to selected mailbox."
  (interactive)
  (himalaya--pick-mailbox
   "Move to mailbox: "
   (lambda (mailbox)
     (let ((prev-point (point))
	   (ids (or himalaya-marked-ids (list (tabulated-list-get-id)))))
       (himalaya--move-messages
	ids
	mailbox
	(lambda (output)
	  (message "%s" (plist-get output :message))
	  (himalaya-unmark-all-envelopes t)
	  (revert-buffer)
	  (goto-char prev-point)))))))

(defun himalaya-delete-marked-messages ()
  "Delete message(s) matching marked envelope(s) (or envelope at
point) from current mailbox of current account. Messages are moved to
the trash, or removed for good when they already are in the trash."
  (interactive)
  (let* ((prev-point (point))
	 (envelope (tabulated-list-get-entry))
         (subject (substring-no-properties (elt envelope 2)))
	 (subject-or-ids (if himalaya-marked-ids (string-join himalaya-marked-ids ", ") subject)))
    (when (y-or-n-p (format "Delete message(s) %s? " subject-or-ids))
      (himalaya--delete-messages
       (or himalaya-marked-ids (tabulated-list-get-id))
       (lambda (output)
	 (message "%s"
                  (pcase (plist-get output :action)
                    ("moved-to-trash" (format "%d message(s) moved to trash" (plist-get output :count)))
                    ("flagged" (format "%d message(s) flagged as deleted, expunge to remove them" (plist-get output :count)))
                    (_ (format "%d message(s) deleted" (plist-get output :count)))))
	 (himalaya-unmark-all-envelopes t)
	 (revert-buffer)
	 (goto-char prev-point))))))

(defun himalaya-send-buffer ()
  "Send the current buffer. If it is a reply, flag the message replied
to as answered."
  (interactive)
  (let ((buffer (current-buffer))
        (reply-to himalaya--compose-reply-to)
        (himalaya-account himalaya--compose-account))
    (himalaya--send-template
     (himalaya--encode-buffer)
     (lambda (output)
       (message "%s" (plist-get output :message))
       (with-current-buffer buffer
         (set-buffer-modified-p nil)
         (kill-buffer))
       (when reply-to
         (let ((himalaya-account himalaya-account)
               (himalaya-mailbox (car reply-to)))
           (himalaya--add-flag (cdr reply-to) "answered" #'ignore)))))))

(defun himalaya-save-draft ()
  "Save the current buffer to the drafts mailbox."
  (interactive)
  (let ((buffer (current-buffer))
        (himalaya-account himalaya--compose-account))
    (himalaya--save-template-draft
     (himalaya--encode-buffer)
     (lambda (_)
       (message "Draft saved")
       (with-current-buffer buffer
         (set-buffer-modified-p nil))))))

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
    (define-key map (kbd "C-c C-d") #'himalaya-save-draft)
    map))

(define-derived-mode himalaya-message-write-mode message-mode "Himalaya-Write"
  "Himalaya message writing mode."
  (setq-local mail-header-separator "")
  (add-hook 'kill-buffer-hook #'himalaya--delete-compose-temp-dirs nil t))

(provide 'himalaya-message)
;;; himalaya-message.el ends here

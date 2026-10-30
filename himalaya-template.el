;;; himalaya-template.el --- Template management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defun himalaya--write-template (callback)
  "Fetch a template for writing a new message using current account,
then call CALLBACK with the raw RFC 5322 template."
  (message "Generating new template…")
  (himalaya--run-raw
   callback
   nil
   "message"
   "compose"
   (when himalaya-account (list "--account" himalaya-account))
   "--body" ""))

(defun himalaya--reply-template (id callback &optional reply-all)
  "Fetch a reply template for message matching envelope ID from
current mailbox on current account, then call CALLBACK with the raw
RFC 5322 template. If REPLY-ALL is non-nil, the template will include
all recipients in To and Cc headers."
  (message "Generating reply template…")
  (himalaya--run-raw
   callback
   nil
   "message"
   "reply"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
   (when reply-all "--all")
   "--body" ""
   (format "%s" id)))

(defun himalaya--forward-template (id callback)
  "Fetch a forward template for message matching envelope ID from
current mailbox on current account, then call CALLBACK with the raw
RFC 5322 template."
  (message "Generating forward template…")
  (himalaya--run-raw
   callback
   nil
   "message"
   "forward"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
   "--body" ""
   (format "%s" id)))

(defun himalaya--send-template (template callback)
  "Send the raw RFC 5322 TEMPLATE using current account. Himalaya CLI
saves a copy according to its `message.send.save-copy' option."
  (message "Sending message…")
  (himalaya--run
   callback
   template
   "message"
   "send"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--save-template-draft (template callback)
  "Add the raw RFC 5322 TEMPLATE to the drafts mailbox of current
account, flagged as draft."
  (message "Saving draft…")
  (himalaya--run
   callback
   template
   "message"
   "add"
   (when himalaya-account (list "--account" himalaya-account))
   "--mailbox" "drafts"
   "--flag" "draft"))

(provide 'himalaya-template)
;;; himalaya-template.el ends here

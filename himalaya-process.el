;;; himalaya-process.el --- Process management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defun himalaya--clear-io-buffers ()
  (with-current-buffer (get-buffer-create "*Himalaya stdout*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (with-current-buffer (get-buffer-create "*Himalaya stderr*")
    (let ((inhibit-read-only t)) (erase-buffer))))

(defun himalaya--build-command (json args)
  "Build the Himalaya CLI command line for ARGS. Global options come
first. If JSON is non-nil, ask for JSON output."
  (cons himalaya-executable
        (flatten-list
         (list (when himalaya-config-path (list "-c" himalaya-config-path))
               (when himalaya-log-level (list "--log-level" himalaya-log-level))
               (when json "--json")
               args))))

(defun himalaya--signal-error (output)
  "Display the stderr buffer and signal a Lisp error. With `--json',
Himalaya CLI prints errors as a JSON object to stdout: use its
message when OUTPUT contains one."
  (display-buffer "*Himalaya stderr*")
  (let ((report (ignore-errors
                  (json-parse-string output :object-type 'plist :array-type 'list))))
    (if (and (listp report) (stringp (plist-get report :error)))
        (error "Himalaya: %s" (string-join (cons (plist-get report :error)
                                                 (plist-get report :sources))
                                           ": "))
      (error "Himalaya exited with a non-zero status"))))

(defun himalaya--start (callback input output args)
  "Asynchronously run Himalaya CLI with ARGS. OUTPUT is `json',
`plain' or `raw', and decides what CALLBACK is called with: the
parsed JSON, the output as a string, or the output as a unibyte
string of raw bytes. INPUT is sent to stdin if not nil, a unibyte
INPUT being sent as is."
  (himalaya--clear-io-buffers)
  (let* ((buffer (get-buffer "*Himalaya stdout*"))
         (command (himalaya--build-command (eq output 'json) args))
         (coding (cons (if (eq output 'raw) 'binary 'utf-8)
                       (if (and input (not (multibyte-string-p input))) 'binary 'utf-8)))
         (sentinel (lambda (process _event) (himalaya--run-sentinel process output callback))))
    (with-current-buffer buffer
      (set-buffer-multibyte (not (eq output 'raw))))
    (let ((process (make-process
                    :name "himalaya"
                    :buffer buffer
                    :stderr (get-buffer "*Himalaya stderr*")
                    :connection-type 'pipe
                    :coding coding
                    :noquery t
                    :command command
                    :sentinel sentinel)))
      (when input
        (process-send-string process input))
      ;; Some commands read stdin when it is a pipe, close it so they
      ;; never wait for it
      (process-send-eof process))))

(defun himalaya--run-sentinel (process output callback)
  "Sentinel function for `himalaya--start' processes."
  (when (memq (process-status process) '(exit signal))
    (message nil)
    (let ((stdout (with-current-buffer (process-buffer process) (buffer-string))))
      (unless (eq 0 (process-exit-status process))
        (himalaya--signal-error stdout))
      (funcall callback
               (if (eq output 'json)
                   (json-parse-string stdout :object-type 'plist :array-type 'list)
                 stdout)))))

(defun himalaya--run (callback input &rest args)
  "Asynchronously run Himalaya CLI with ARGS. CALLBACK is called with
the parsed JSON output when Himalaya CLI exits. INPUT is sent to stdin
if not nil. Signals a Lisp error and displays the output on non-zero
exit."
  (himalaya--start callback input 'json args))

(defun himalaya--run-plain (callback input &rest args)
  "Like `himalaya--run', but CALLBACK is called with the plain text
output."
  (himalaya--start callback input 'plain args))

(defun himalaya--run-raw (callback input &rest args)
  "Like `himalaya--run', but CALLBACK is called with the output as a
unibyte string, for raw RFC 5322 messages."
  (himalaya--start callback input 'raw args))

(defun himalaya--run-blocking (&rest args)
  "Blocking version of `himalaya--run'."
  (himalaya--clear-io-buffers)
  (let ((stderr-file (make-temp-file "himalaya-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((command (himalaya--build-command t args))
                 (coding-system-for-read 'utf-8)
                 (exit-status (apply #'call-process (car command) nil (list t stderr-file) nil (cdr command)))
                 (output (buffer-string)))
            (with-current-buffer "*Himalaya stderr*"
              (insert-file-contents stderr-file))
            (unless (eq 0 exit-status)
              (himalaya--signal-error output))
            (json-parse-string output :object-type 'plist :array-type 'list)))
      (delete-file stderr-file))))

(provide 'himalaya-process)
;;; himalaya-process.el ends here

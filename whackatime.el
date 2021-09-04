;;; whackatime.el --- Track time in emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020  Free Software Foundation, Inc.

;; Author: Mark Dawson <markgdawson@gmail.com>
;; Maintainer: Mark Dawson <markgdawson@gmail.com>
;; URL: https://github.com/markgdawson/whackatime
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (magit "2.90.1"))
;; Keywords: tools

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Whackatime is intended to record events in a log.

;;; Code:

(require 'magit)

(defvar whackatime--last-buffer nil)

(defvar whackatime-log-filename (concat user-emacs-directory
                                        "whackatime-log.rec")
  "Buffer to log whackatime events.")

(defun whackatime-turn-on ()
  "Watch for activity in buffers."
  (add-hook 'buffer-list-update-hook #'whackatime-buffer-list-update-hook nil t)
  ;; Fire buffer list update hook once at startup to ensure current
  ;; buffer is recorded.
  (whackatime-buffer-list-update-hook)
  (whackatime--start-idle-timer))

(defun whackatime-turn-off ()
  "Stop watching for activity in buffers."
  (remove-hook 'buffer-list-update-hook #'whackatime-buffer-list-update-hook t))

(defun whackatime--ordinary-buffer-p (buff)
  "Return non-nil if BUFF is an ordinary file-visiting buffer."
  (and
   (buffer-file-name buff)
   (not (auto-save-file-name-p (buffer-file-name buff)))))

(defun whackatime--git-commit (buff)
  "Fetch git commit associated with a BUFF or nil."
  (with-current-buffer buff
    (when (file-exists-p default-directory)
      (magit-git-string "log" "-n1" "--format=%h"))))

(defun whackatime--git-branch (buff)
  "Fetch git commit associated with a BUFF or nil."
  (with-current-buffer buff
    (when (file-exists-p default-directory)
      (magit-git-string "branch" "--show-current"))))

(defun whackatime--current-buffer-is-firefox-private-mode ()
  "Return non-nil if current buffer is a firefox private mode buffer."
  (and (bound-and-true-p exwm-title) exwm-title (string-match " (Private Browsing)$" exwm-title)))

(defun whackatime-recordable-major-mode (buffer)
  "Return the recordable major mode for BUFFER.
The mode name is lowercase with no spaces."
  (with-current-buffer buffer
    (cond ((bound-and-true-p exwm-title)
           (downcase
            (string-replace " " "-" (concat "exwm-" exwm-class-name))))
          (major-mode))))

(defun whackatime-laptop-lid-status-changed (status)
  "This should be called by a mechanism like ACPI ever time the laptop lid is closed or opened.  STATUS should have value \"closed\" when the lid is open."
  (if (string= status "closed")
      (whackatime-log-idle)
    (whackatime-log-active)))

(defun whackatime-recordable-buffer-name (buff)
  "Return buffer name for buffer BUFF."
  (cond ((whackatime--ordinary-buffer-p buff) (buffer-file-name buff))
        ;; org edit src buffer
        ((bound-and-true-p org-src--beg-marker) (buffer-file-name (marker-buffer org-src--beg-marker)))
        ((whackatime--current-buffer-is-firefox-private-mode) "Firefox - Private")
        ((bound-and-true-p exwm-title) exwm-title)
        ((string= major-mode 'dired-mode) default-directory)
        ((buffer-name buff))))

(defun whackatime-log-message (message)
  "Log the activity with MESSAGE."
  (append-to-file message nil whackatime-log-filename))

(defun whackatime-log-activity (buffer)
  "Log whackatime activity for BUFFER."
  (whackatime-log-message
   (format "Timestamp: %f\nGitBranch: %s\nGitCommit: %s\nMajorMode: %s\nBufferName: %s\n"
           (float-time)
           (whackatime--git-branch buffer)
           (whackatime--git-commit buffer)
           (whackatime-recordable-major-mode buffer)
           (whackatime-recordable-buffer-name buffer))))

(defun whackatime-buffer-list-update-hook ()
  "Called on BUFFER-LIST-UPDATE-HOOK to process current buffer."
  (whackatime-process-buffer (current-buffer)))

(defcustom whackatime-ignore-buffer-regexp
  "^\s*\\*.*[^*]\\*$"
  "Buffer names matching this regular expression are ignored.")

(defun whackatime-ignore-buffer-p (buffer)
  "Return non-nil if BUFFER is a special buffer."
  (string-match whackatime-ignore-buffer-regexp
                (buffer-name buffer)))

(defun whackatime-process-buffer (buffer)
  "Determine if BUFFER is a different buffer to the last call, and log an event if it is."
  (unless (or (equal whackatime--last-buffer buffer)
              (window-minibuffer-p)
              (whackatime-ignore-buffer-p buffer))
    (whackatime-log-activity buffer)
    (setq whackatime--last-buffer buffer)))

;; Idle timer and notifications

(defvar whackatime--idle-time-timer nil
  "Time for recording idle time.")

(defvar whackatime-idle-check-seconds 5
  "Number of seconds between checks for idle state.")

(defvar whackatime-idle-timeout-seconds 60
  "Number of seconds to classify as idle.")

(defvar whackatime--idle-status nil)

(defun whackatime-log-idle ()
  "Log last Emacs idle state."
  (whackatime-log-message
   (format "%f nil nil IDLE" (- (float-time) (whackatime-idle-seconds)))))

(defun whackatime-log-active ()
  "Log that Emacs is now active."
  (when whackatime--last-buffer
    (whackatime-log-activity whackatime--last-buffer)))

(defun whackatime-idle-seconds ()
  "Return the number of seconds for which Emacs has been idle."
  (if-let (idle-lisp (current-idle-time))
      (time-convert idle-lisp 'integer)
    0))

(defun whackatime-idle-p ()
  "Check if number of idle seconds is greater than idle threshold."
  (> (whackatime-idle-seconds)
     whackatime-idle-timeout-seconds))

(defun whackatime--check-idle-time ()
  "Run every WHACKATIME-IDLE-CHECK-SECONDS to check idle state.
This also runs before each whackatime logging message, to ensure that
active state appears in the log before a buffer change."
  (let ((idle-status (whackatime-idle-p)))
    (unless (eql idle-status whackatime--idle-status)
      (if idle-status
          (whackatime-log-idle)
        (whackatime-log-active))
      (setq whackatime--idle-status idle-status))))

(defun whackatime--stop-idle-timer ()
  "Stop the timer for checking idle status."
  (when (timerp whackatime--idle-time-timer)
    (setq whackatime--idle-time-timer
          (cancel-timer whackatime--idle-time-timer))))

(defun whackatime--start-idle-timer ()
  "Start the timer for checking idle status."
  (whackatime--stop-idle-timer)
  (setq whackatime--idle-time-timer
        (run-at-time 0 whackatime-idle-check-seconds #'whackatime--check-idle-time)))

;;;###autoload
(define-minor-mode whackatime-mode
  "Toggle whackatime-mode."
  :lighter    " whacka"
  :init-value nil
  :global     nil
  :group      'whackatime
  (cond
   (noninteractive (setq whackatime-mode nil))
   (whackatime-mode (whackatime-turn-on))
   (t (whackatime-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-whackatime-mode
  whackatime-mode
  (lambda () (whackatime-mode 1)))

(provide 'whackatime)
;;; whackatime.el ends here

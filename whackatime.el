;;; whackatime.el --- Track time in emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020  Free Software Foundation, Inc.

;; Author: Mark Dawson <markgdawson@gmail.com>
;; Maintainer: Mark Dawson <markgdawson@gmail.com>
;; URL: https://github.com/markgdawson/whackatime
;; Version: 0.0.1
;; Package-Requires: ((avy "0.5.0"))
;; Keywords: window, location

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
(defvar whackatime--last-buffer nil)

(defvar whackatime-log-buffer (pop-to-buffer "*whackatime-log*")
  "Buffer to log whackatime events.")

(defun whackatime-turn-on ()
  "Watch for activity in buffers."
  (add-hook 'buffer-list-update-hook #'whackatime-buffer-list-update-hook nil t))

(defun whackatime-turn-off ()
  "Stop watching for activity in buffers."
  (remove-hook 'buffer-list-update-hook #'whackatime-buffer-list-update-hook t))

(defun whackatime--ordinary-buffer-p (buff)
  "Return non-nil if BUFF is an ordinary file-visiting buffer."
  (and
   (buffer-file-name buff)
   (not (auto-save-file-name-p (buffer-file-name buff)))))

(defun whackatime--git-commit (&optional buff)
  (with-current-buffer (or buff (current-buffer))
    (when (file-exists-p default-directory)
      (magit-git-string "log" "-n1" "--format=%h"))))

(defun whackatime-recordable-buffer-name (buff)
  "Return buffer name if BUFF is recordable."
  (cond ((whackatime--ordinary-buffer-p buff) (buffer-file-name buff))
        ;; org edit src buffer
        ((bound-and-true-p org-src--beg-marker) (buffer-file-name (marker-buffer org-src--beg-marker)))
        ((bound-and-true-p exwm-title) exwm-title)
        ((string= major-mode 'dired-mode) default-directory)
        ((buffer-name buff))))

(defun whackatime-log-message (message)
  "Log the activity with BUFFER-NAME."
  (save-excursion
    (with-current-buffer whackatime-log-buffer
      (goto-char (point-max))
      (insert message)
      (insert "\n"))))

(defun whackatime-log-activity (buffer)
  "Log whackatime activity for BUFFER."
  (whackatime-log-message
   (format "%f %s %s %s"
           (float-time)
           (whackatime--git-commit buffer)
           major-mode
           (whackatime-recordable-buffer-name buffer))))

(defun whackatime-buffer-list-update-hook ()
  "Called on BUFFER-LIST-UPDATE-HOOK to process current buffer."
  (whackatime-process-buffer (current-buffer)))

(defun whackatime-ignore-buffer-p (buffer)
  "Return non-nil if BUFFER should be ignored."
  (string-match "\*eldoc for"
                (buffer-name buffer)))

(defun whackatime-process-buffer (buffer)
  "Determine if BUFFER is a different buffer to the last call, and log an event if it is."
  (unless (or (equal whackatime--last-buffer buffer)
              (window-minibuffer-p)
              (whackatime-ignore-buffer-p buffer))
    (whackatime-log-activity buffer)
    (setq whackatime--last-buffer buffer)))

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

(define-globalized-minor-mode global-whackatime-mode whackatime-mode (lambda () (whackatime-mode 1)))

(provide 'whackatime)
;;; whackatime ends here

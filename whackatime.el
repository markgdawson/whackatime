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
  (add-hook 'post-command-hook #'whackatime-post-command-hook nil t))

(defun whackatime-turn-off ()
  "Stop watching for activity in buffers."
  (remove-hook 'post-command-hook #'whackatime-post-command-hook t))

(defun whackatime--ordinary-buffer-p (buff)
  "Return non-nil if BUFF is an ordinary file-visiting buffer."
  (and
   (buffer-file-name buff)
   (not (auto-save-file-name-p (buffer-file-name buff)))))

(defun whackatime-recordable-buffer (buff)
  "Return buffer name if BUFF is recordable."
  (cond ((whackatime--ordinary-buffer-p buff) (buffer-file-name buff))
        ;; org edit src buffer
        (org-src--beg-marker (buffer-file-name (marker-buffer org-src--beg-marker)))
        (exwm-title exwm-title)
        ((window-minibuffer-p) nil)
        ((string= major-mode 'dired-mode) default-directory)
        ((buffer-name buff))))

(defun whackatime-log-activity-maybe ()
  "Potentially record whackatime log."
  (let* ((buffer (current-buffer))
         (buffer-name buffer))
    (when-let (buffer-name (whackatime-recordable-buffer buffer))
      (whackatime-log-activity buffer-name)
      (setq whackatime--last-buffer buffer))))

(defun whackatime-buffer-change ()
  "Called when a command will switch current buffer."
  (whackatime-log-activity-maybe))

(defvar whackatime-log-buffer (pop-to-buffer "*whackatime-log*")
  "Buffer to log whackatime events")

(defun whackatime-log-activity (buffer-name)
  "Log the activity with a LABEL and a BUFFER."
  (with-current-buffer whackatime-log-buffer
    (goto-char (point-max))
    (insert (format "%f: %s %s\n" (float-time) major-mode buffer-name))))

;;;###autoload
(define-minor-mode whackatime-mode
  "Toggle WhackaTime"
  :lighter    " whacka"
  :init-value nil
  :global     nil
  :group      'whackatime
  (cond
   (noninteractive (setq whackatime-mode nil))
   (whackatime-mode (whackatime-turn-on))
   (t (whackatime-turn-off))))

(define-globalized-minor-mode global-whackatime-mode whackatime-mode (lambda () (whackatime-mode 1)))

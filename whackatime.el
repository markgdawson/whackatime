(defvar whackatime--last-buffer nil)

(defcustom whackatime-current-buffer-changed-hook nil
  "Functions to run when current buffer has changed.")

(defun whackatime-turn-on ()
  "Watch for activity in buffers."
  (add-hook 'post-command-hook #'whackatime-fire-buffer-changed-hook nil t)
  (add-hook 'whackatime-current-buffer-changed-hook #'whackatime-buffer-change nil t))

(defun whackatime-turn-off ()
  "Stop watching for activity in buffers."
  (remove-hook 'post-command-hook #'whackatime-fire-buffer-changed-hook t)
  (remove-hook 'whackatime-current-buffer-changed-hook #'whackatime-buffer-change t))

(defun whackatime-fire-buffer-changed-hook ()
  "Fire `whackatime-current-buffer-changed-hook` on buffer change."
  (let ((buffer (current-buffer)))
    (unless (equal whackatime--last-buffer buffer)
      (progn
        (run-hooks 'whackatime-current-buffer-changed-hook)))))

(defun whackatime--ordinary-buffer-p (buff)
  (and
   (buffer-file-name buff)
   (not (auto-save-file-name-p (buffer-file-name buff)))))

(defun whackatime-recordable-buffer (buff)
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

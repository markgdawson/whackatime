(defun whackatime--read-current-line (buffer)
  "Return the current line in BUFFER as a string."
  (with-current-buffer buffer
    (buffer-substring-no-properties (or (beginning-of-line) (point))
                                    (or (end-of-line) (point)))))

(defun whackatime-record->event-time (record-str)
  "Return the event time from the whackatime record RECORD-STR."
  (string-to-number (car (split-string record-str " "))))

(defun whackatime-record->buffer-name (record-str)
  "Return the recordable buffer name from RECORD-STR."
  (replace-regexp-in-string "^\\(.*? \\)\\{3\\}" "" record-str))

(defun whackatime-next-record-remaining-p (buffer)
  "Move to next record in BUFFER, returning non-nil if there are records remaining after the one that is moved to.
This assumes that BUFFER ends on the last line or an empty line"
  (with-current-buffer buffer
    (forward-line 1)
    (not (= (point) (point-max)))))

(defun whackatime-first-record (buffer)
  "Move to the first record in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))))

(defun whackatime-process-log-by-line (fn time-now context log-buffer)
  "Process LOG-BUFFER line by line calling FN on each iteration.

FN is a function of three arguments. The duration of this event (i.e. the time between this even and the next event), the record of this even as a string (as it appears in the whackatime log) and the CONTEXT.

CONTEXT is threaded through calls to FN, and can be used to track state.

The duration of the latest record is the time between the last even time and TIME-NOW."
  (let ((last-line nil)
        (current-line nil))
    (whackatime-first-record log-buffer)
    (setq current-line (whackatime--read-current-line log-buffer))
    (while (whackatime-next-record-remaining-p log-buffer)
      (setq last-line current-line)
      (setq current-line (whackatime--read-current-line log-buffer))
      (setq context (funcall fn (- (whackatime-record->event-time current-line)
                                   (whackatime-record->event-time last-line))
                             last-line
                             context)))
    (funcall fn (- time-now
                   (whackatime-record->event-time current-line))
             current-line
             context)))

(defun whackatime--debug--log-processing ()
  "Show the arguments passed to the whackatime-process-log-by-line function.
Used for debugging purposes."
  (let ((buffer (get-buffer-create "*whackatime-tmp*")))
    (with-current-buffer buffer
      (erase-buffer)
      (whackatime-process-log-by-line (lambda (duration line context)
                                        (insert (concat (number-to-string duration)
                                                        " -> "
                                                        line
                                                        "\n")))
                                      (float-time)
                                      nil
                                      whackatime-log-buffer))
    (pop-to-buffer buffer)))

(defun whackatime-format-time (seconds)
  "Return SECONDS printed in human readable form."
  (let ((s (float seconds)))
    (cond ((< s 60) (format "%s s" s))
          ((< s (* 60 60)) (format "%.2f mins" (/ s 60)))
          ((< s (* 60 60 24)) (format "%.2f hours" (/ s 60 60)))
          ((format "%.2f days" (/ s 60 60 24))))))

(defun whackatime-process--accumulate-durations (duration current-line result-hash-table)
  "Return RESULT-HASH-TABLE with DURATION added to the entry for CURRENT-LINE.

For use with WHACKATIME-PROCESS-LOG-BY-LINE function to accumulate total durations in a buffer.

DURATION is the number of seconds for this event, and CURRENT-LINE is the current line record for this event.  RESULT-HASH stores the resultant accumulation."
  (let ((key (whackatime-record->buffer-name current-line)))
    (puthash key
             (+ duration (or (gethash key result-hash-table) 0))
             result-hash-table)
    result-hash-table))

(defun whackatime-buffer-name-time-summary ()
  "Show summary of times spent in each buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*whackatime-buffer-time-summary*"))
        (result (whackatime-process-log-by-line
                 #'whackatime-process--accumulate-durations
                 (float-time)
                 (make-hash-table :test 'equal)
                 whackatime-log-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (maphash (lambda (buffer-name duration)
                 (if (> duration 0.1)
                     (insert (format "%s : %s\n"
                                     (whackatime-format-time duration)
                                     buffer-name))))
               result))
    (pop-to-buffer buffer)))

(provide 'whackatime-read)

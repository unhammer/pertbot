(defvar pertbot-seen (make-hash-table :test 'equal))

(defun pertbot-see (nick)
  (message "seeing %s" nick)
  (puthash (erc-downcase nick) (current-time) pertbot-seen))

(defun pertbot-handle-seen (nickuserhost msg)
  (if (string-match "^\\s *,seen\\s +\\(\\S +\\)\\s *" msg)
      (let* ((nick (match-string-no-properties 1 msg))
	     (time (gethash (erc-downcase nick) pertbot-seen)))
	(cond ((gethash (erc-downcase nick) erc-channel-users)
	       (erc-send-message (format "%s is here now!" nick)))
	      (time (erc-send-message (format "%s last seen %s ago"
					      nick
					      (pertbot-time-duration
					       (time-to-seconds (time-since time))))))
	      (t (erc-send-message (format "Haven't seen %s around lately" nick)))))
    (erc-send-message "Usage: ,seen NICK")))

(defun pertbot-seen-handle-channel-members-updated ()
  "Add to `erc-channel-members-updated-hook' to turn on seen-recording."
  (when (boundp 'nick)
    (pertbot-see nick)))

(add-hook 'erc-channel-members-changed-hook
	  #'pertbot-seen-handle-channel-members-updated)

(defun pertbot-seen-insert-post-hook ()
  "Unfortunately we can't simply have a join hook, since names
are received after join."
  (goto-char (or (erc-find-parsed-property) (point-min)))
  (let* ((parsed (erc-get-parsed-vector (point))))
    (when (and parsed
	       (equal "353" (erc-response.command parsed))) ; NAMES
      (mapc
       'pertbot-see
       (split-string (erc-response.contents parsed))))))

(add-hook 'erc-insert-post-hook
	  #'pertbot-seen-insert-post-hook)

(defconst pertbot-time-duration-words
  (list	(cons "second" 1)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds.")

(defun pertbot-time-duration (secs)
  "Take number of seconds specified by `secs', return relative
time as string."
  (defun biggest-unit (secs)
    (setq words pertbot-time-duration-words)
    (while (and (cdr words)
		(>= secs (cdar (cdr words))))
      (setq words (cdr words)))
    (car words))  
  (if (eq secs 0)
      "0 seconds"
    (let (str)
      (while (> secs 0)
	(let* ((unit (biggest-unit secs))
	       (count-unit (floor (/ secs  (cdr unit))))
	       (count-secs (* count-unit (cdr unit))))
	  (if (<= count-unit 0)
	      (setq secs 0)
	    (setq	   
	     str (concat str
			 (when str ", ")
			 (format "%s %s" count-unit (car unit))
			 (when (not (eq 1 count-unit)) "s")))
	    (setq secs (- secs count-secs)))))
      str)))

(provide 'pertbot-seen)

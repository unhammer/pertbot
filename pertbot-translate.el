(defvar pertbot-installed-pairs-cached nil "List of installed pairs")
(defun pertbot-installed-pairs ()
  (if pertbot-installed-pairs-cached
      pertbot-installed-pairs-cached
    (setq pertbot-installed-pairs-cached
	  (split-string (shell-command-to-string "apertium -l")))))

(defun pertbot-handle-installed (msg)
  (erc-send-message (format "Installed translation directions: %s"
			    (apply #'concat
				   (mapcar (lambda (key)
					     (format "%s " key))
					   (pertbot-installed-pairs))))))
(defun pertbot-handle-translate (msg)
  (if (string-match "^\\s *,translate\\s +\\(\\S +-\\S +*\\)\\s +\\(.*\\)\n" msg)
      (let* ((pair (match-string 1 msg))
	     (input (match-string 2 msg)))
	(if (member pair (pertbot-installed-pairs))
	    (let ((output
		   (with-temp-buffer
		     (insert input)
		     (if (eq 0 (shell-command-on-region
				(point-min) (point-max)
				(concat "apertium " pair)
				nil 'replace "*apertium-error*"))
			 (buffer-substring-no-properties (point-min) (point-max))
		       nil))))
	      (if output
		  (erc-send-message (concat pair ": " output))
		(erc-send-message "Translation failed, for some reason.")))
	  (erc-send-message "I don't have that language pair installed. Try ,installed")))
    (erc-send-message "Usage: ,translate DIRECTION")
    (erc-send-message "where DIRECTION is an apertium language pair direction, e.g. es-ca")))


(provide 'pertbot-translate)

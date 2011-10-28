(defvar pertbot-installed-dirs-cached nil "List of installed pairs")
(defun pertbot-installed-dirs ()
  (if pertbot-installed-dirs-cached
      pertbot-installed-dirs-cached
    (setq pertbot-installed-dirs-cached
	  (split-string (shell-command-to-string "apertium -l")))))

(defun pertbot-handle-installed (msg)
  (erc-send-message (format "Installed translation directions: %s"
			    (apply #'concat
				   (mapcar (lambda (key)
					     (format "%s " key))
					   (pertbot-installed-dirs))))))

(defun pertbot-translate-helper (dir input)
  (with-temp-buffer
    (insert input)
    (if (eq 0 (shell-command-on-region
	       (point-min) (point-max)
	       (concat "apertium " dir)
	       nil 'replace "*apertium-error*"))
	(buffer-substring-no-properties (point-min) (point-max))
      nil)))


(defun pertbot-handle-translate (msg)
  (if (string-match "^\\s *,translate\\s +\\(\\S +-\\S +*\\)\\s +\\(.*\\)\n" msg)
      (let* ((dir (match-string 1 msg))
	     (input (match-string 2 msg)))
	(if (member dir (pertbot-installed-dirs))
	    (let ((output (pertbot-translate-helper dir input)))
	      (if output
		  (erc-send-message (concat dir ": " output))
		(erc-send-message "Translation failed, for some reason.")))
	  (erc-send-message "I don't have that language direction installed. Try ,installed")))
    (erc-send-message "Usage: ,translate DIRECTION")
    (erc-send-message "where DIRECTION is an apertium language pair direction, e.g. es-ca")))

(defun unassoc (key list)
  "Return `list' without the pair that has the key `key'."
  (remove t
	  (mapcar (lambda (p) (or (equal key (car p))
				  p))
		  list)))

(defvar pertbot-followed (make-hash-table :test 'equal))
(defun pertbot-handle-follow (nickuserhost msg)
  (if (string-match "^\\s *,follow\\s +\\(\\S +\\)\\s +\\(\\S +-\\S +*\\)\\s *\n" msg)
      (let* ((followed (match-string 1 msg))
	     (dir (match-string 2 msg))
	     (follower (car (split-string nickuserhost "!"))))
	(if (member dir (pertbot-installed-dirs))
	    (let ((otherfollowers (unassoc follower
					   (gethash followed pertbot-followed))))
	      (add-to-list 'erc-pals followed)
	      (puthash followed
		       (cons (cons follower dir)
			     otherfollowers)
		       pertbot-followed)
	      (erc-send-message (format "Translating %s in the direction %s for %s" followed dir follower)))
	    (erc-send-message "I don't have that language direction installed. Try ,installed")))
    (erc-send-message "Usage: ,follow NICK DIRECTION")
    (erc-send-message "where DIRECTION is an apertium language pair direction, e.g. es-ca")))
(defun pertbot-handle-unfollow (nickuserhost msg)
  (if (string-match "^\\s *,unfollow\\s +\\(\\S +\\)\\s *\n" msg)
      (let* ((followed (match-string 1 msg))
	     (follower (car (split-string nickuserhost "!")))
	     (otherfollowers (unassoc follower
				      (gethash followed pertbot-followed))))
	(unless otherfollowers
	  (remove followed erc-pals))
	(puthash followed
		 otherfollowers
		 pertbot-followed)
	(erc-send-message (format "Unfollowing %s for %s" followed follower)))
    (erc-send-message "Usage: ,unfollow NICK")))

(defun pertbot-handle-follow-match (nickuserhost msg)
  (let* ((followed (car (split-string nickuserhost "!")))
	 (followers (gethash followed pertbot-followed)))
    (mapc (lambda (f-d)
	    (let ((follower (car f-d))
		  (dir (cdr f-d)))
	      (when (gethash follower erc-channel-users)
		(let ((output (pertbot-translate-helper dir msg)))
		  (when output
		    (erc-cmd-MSG (concat follower " " output)))))))
	  followers)))

(provide 'pertbot-translate)

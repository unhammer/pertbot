(require 'iso-639)
(require 'pertbot-apertium-pairs)
(require 'pertbot-translate)
(require 'pertbot-seen)

(setq erc-keywords '(",seen" ",iso" ",help" ",commands" ",pairs" ",translate" ",installed" ",follow" ",unfollow"))

(defun pertbot-handle-help (msg)
  (erc-send-message "Try ,commands to see a full list of commands. The most useful are probably ,iso and ,pairs and ,seen."))

(defun pertbot-handle-commands (msg)
  (erc-send-message (format "Known commands: %s"
			    (apply #'concat
				   (mapcar (lambda (key)
					     (format "%s " key))
					   erc-keywords)))))

(defun pertbot-handle-iso (msg)
  (if (string-match "^\\Sw*,iso\\Sw+\\(...?\\)\\Sw*\n" msg)
      (let* ((code (match-string-no-properties 1 msg))
	     (lang (assoc code iso-639-language-codes))
	     (reply (if lang
			(format "ISO language code \"%s\" is %s" code (cdr lang))
		      (format "Unknown ISO language code: %s" code))))
	(erc-send-message reply))
    (erc-send-message "Usage: ,iso CODE")
    (erc-send-message "where CODE is a two- or three-letter ISO-639 language code.")))

(defun pertbot-handle-match (match-type nickuserhost msg)
  (cond ((eq match-type 'keyword)
	 (when (string-match "^\\Sw*,\\(\\sw+\\)" msg)
	   (let* ((key (downcase (match-string-no-properties 1 msg)))
		  (simple-answer (assoc key pertbot-simple-answers)))
	     (cond ((equal key "iso") (pertbot-handle-iso msg))
		   ;; TODO: use the method in `pcomplete-erc-commands'
		   ;; for auto-creating these functions with
		   ;; `apropos-internal'
		   ((equal key "help") (pertbot-handle-help msg))
		   ((equal key "commands") (pertbot-handle-commands msg))
		   ((equal key "pairs") (pertbot-handle-pairs msg))
		   ((equal key "translate") (pertbot-handle-translate msg))
		   ((equal key "installed") (pertbot-handle-installed msg))
		   ((equal key "follow") (pertbot-handle-follow nickuserhost msg))
		   ((equal key "unfollow") (pertbot-handle-unfollow nickuserhost msg))
		   ((equal key "seen") (pertbot-handle-seen nickuserhost msg))
		   (simple-answer
		    (erc-send-message (cdr simple-answer)))))))
	((eq match-type 'pal) (pertbot-handle-follow-match nickuserhost msg))
	((and (eq match-type 'current-nick)
	      (string-match (concat "^" erc-nick) msg))
	 (erc-send-message "hi there"))))
(add-hook 'erc-text-matched-hook 'pertbot-handle-match)


  

(defvar pertbot-simple-answers
  '(("hi" . "Hello there"))
  "Association list of questions and answers, simple strings (no
  functions). Meant to be overridden by the user. Unlike
  `erc-keywords', no initial comma.")

;; User should define pertbot-simple-answers before requiring this:
(mapc (lambda (key) (add-to-list 'erc-keywords (concat "," key)))
      (mapcar #'car pertbot-simple-answers))

(provide 'pertbot)

(require 'iso-639)
(require 'pertbot-apertium-pairs)

(setq erc-keywords '("iso" "help" "commands" "pairs"))

(defun pertbot-handle-help (msg)
  (erc-send-message "I've just been born so I don't know many commands. Try ,commands for a full list."))

(defun pertbot-handle-commands (msg)
  (erc-send-message (format "Known commands: %s"
			    (apply #'concat
				   (mapcar (lambda (key)
					     (format ",%s " key))
					   erc-keywords)))))

(defun pertbot-handle-iso (msg)
  (if (string-match "^\\Sw*,iso\\Sw+\\(...?\\)\\Sw*\n" msg)
      (let* ((code (match-string 1 msg))
	     (lang (assoc code iso-639-language-codes))
	     (reply (if lang
			(format "ISO language code \"%s\" is %s" code (cdr lang))
		      (format "Unknown ISO language code: %s" code))))
	(erc-send-message reply))
    (erc-send-message "Usage: ,iso CODE")
    (erc-send-message "where CODE is a two- or three-letter ISO-639 language code.")))

(defun pertbot-handle-match (match-type nickuserhost msg)
  (cond ((eq match-type 'keyword)
	 (string-match "^\\Sw*,\\Sw*\\(\\sw+\\)" msg)
	 (let* ((key (downcase (match-string 1 msg)))
		(simple-answer (assoc key pertbot-simple-answers)))
	   (cond ((equal key "iso") (pertbot-handle-iso msg))
		 ((equal key "help") (pertbot-handle-help msg))
		 ((equal key "commands") (pertbot-handle-commands msg))
		 (simple-answer
		  (erc-send-message (cdr simple-answer)))
		 ((equal key "pairs") (pertbot-handle-pairs msg)))))
	;; guess this matches even if its us saying something :-/
	;; ((eq match-type 'current-nick) (erc-send-message "hi there"))
	))
(add-hook 'erc-text-matched-hook 'pertbot-handle-match)


  

(defvar pertbot-simple-answers
  '(("hi" . "Hello there"))
  "Association list of questions and answers, simple strings (no functions). Meant to be overridden by the user")

;; User should define pertbot-simple-answers before requiring this:
(mapc (lambda (key) (add-to-list 'erc-keywords key))
      (mapcar #'car pertbot-simple-answers))

(provide 'pertbot)

(require 'iso-639)

(setq erc-keywords '(",iso"))

(defun pertbot-handle-iso (msg)
  (if (string-match ",iso \\(...?\\)\n" msg)
      (let* ((code (match-string 1 msg))
	     (lang (assoc code iso-639-language-codes))
	     (reply (if lang
			(format "ISO language code \"%s\" is %s" code (cdr lang))
		      (format "Unknown ISO language code: %s" code))))
	(erc-send-message reply))
    (erc-send-message "Uhwhatnow?")))

(defun pertbot-handle-match (match-type nickuserhost msg)
  (cond ((eq match-type 'keyword)
	 (cond ((equal ",iso" (substring-no-properties msg 0 4))
		(pertbot-handle-iso msg))))
	;; guess this matches even if its us saying something :-/
	;; ((eq match-type 'current-nick) (erc-send-message "hi there"))
	))


(add-hook 'erc-text-matched-hook 'pertbot-handle-match)

(provide 'pertbot)

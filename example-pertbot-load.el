;; Change these values:
(setq erc-nick "pertbot")
(setq erc-password "somedifficultpassword")
(setq erc-server "irc.freenode.net")
(add-to-list 'load-path "~/pertbot")	; path to this directory

(setq erc-autojoin-channels-alist '(("freenode.net" "##test")))

(setq pertbot-simple-answers
  '(("hi" . "Yo!")
    ("ciao" . "ciao")))

;; Leave these:
(require 'pertbot)
(erc-select :password erc-password)

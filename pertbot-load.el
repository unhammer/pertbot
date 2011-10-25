;; Change these values:
(defvar erc-nick "pertbot")
(defvar erc-password "somedifficultpassword")
(defvar erc-server "irc.freenode.net")
(add-to-list 'load-path "~/pertbot")	; path to this directory

(setq erc-autojoin-channels-alist '(("freenode.net" "##test")))

;; Leave these:
(require 'pertbot)
(erc-select :password erc-password)

;; Change these values:
(defvar erc-nick "pertbot")
(defvar erc-password "somedifficultpassword")
(defvar erc-server "irc.freenode.net")
(add-to-list 'load-path "~/pertbot")	; path to this directory

;; Leave these:
(require 'pertbot)
(erc-select)

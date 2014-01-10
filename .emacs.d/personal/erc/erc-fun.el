;;; much taken from https://github.com/bigclean/dotfiles/blob/0ad07f993bcc18b29f8d98380df774a026284744/.emacs.d/conf/bigclean-erc.el

(setq erc-nick "zeeshanlakhani")
(setq erc-away-name "zeeshanlakhani_away")
(setq erc-user-full-name "Zeeshan Lakhani")
(setq erc-prompt ">")

;; spell check
(erc-spelling-mode 1)
(setq erc-autojoin-mode t)
(setq erc-autoaway-mode t)
(setq erc-join-buffer 'bury)
(setq erc-server-auto-reconnect t)

;; erc fill settings
;; right align nickname
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 15)

;; erc timestamp settings
(setq erc-timestamp-right-column 110)
(setq erc-timestamp-format "[%H:%M:%S]")
(setq erc-timestamp-format-right " [%H:%M:%S]")
;; right insert timestamp
(setq erc-insert-timestamp-function 'erc-insert-timestamp-right)
;; right insert away timestamp
(setq erc-insert-away-timestamp-function 'erc-insert-timestamp-right)

;; display channel info in mode-line
(setq erc-track-position-in-mode-line t)
;; don't track server buffer
(setq erc-track-exclude-server-buffer t)
;; Don't track join/quit
(setq erc-track-exclude-types '("NICK" "333" "353" "JOIN" "PART" "QUIT"))
(setq erc-server-coding-system 'utf-8)

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "zeeshanlakhani"
      erc-user-full-name "Zeeshan Lakhani"
      erc-prompt-for-password nil)
;; autojoin channels
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#marksy")
        ("freenode.net" "#coderdojonyc")))

;; logging settings
(setq erc-enable-logging t
      erc-log-mode nil                                  ; close log mode
      erc-log-channels-directory  "~/.emacs.d/personal/erc/logs"
      erc-save-buffer-on-part t ; logs automatically written when you part or quit a channel
      erc-log-file-coding-system 'utf-8)

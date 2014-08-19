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
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-server-coding-system 'utf-8)

(defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
     "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
     (fset command
           `(lambda (arg)
              (interactive "p")
              (if (not (= 1 arg))
                  (call-interactively 'erc)
                (let ((erc-connect-function ',(if ssl
                                                  'erc-open-ssl-stream
                                                'open-network-stream)))
                  (erc :server ,server :port ,port :nick ,nick :password ,pass))))))

;; Create ERC command to connect to Freenode servers
(asf-erc-bouncer-connect erc-freenode "irc.freenode.net" 6667 "zeeshanlakhani" nil "zeeshanlakhani")

(defmacro erc-autojoin (&rest args)
  `(add-hook 'erc-after-connect
             '(lambda (server nick)
                (cond
                 ,@(mapcar (lambda (servers+channels)
                             (let ((servers (car servers+channels))
                                   (channels (cdr servers+channels)))
                               `((member erc-session-server ',servers)
                                 (mapc 'erc-join-channel ',channels))))
                              args)))))


(setq erc-prompt-for-nickserv-password nil)
(setq erc-server-auto-reconnect t
      erc-server-reconnect-timeout 20
      erc-server-reconnect-attempts 3)

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

;; logging settings
(setq erc-enable-logging t
      erc-log-mode nil                                  ; close log mode
      erc-log-channels-directory  "~/.emacs.d/personal/erc/logs"
      erc-save-buffer-on-part t ; logs automatically written when you part or quit a channel
      erc-log-file-coding-system 'utf-8)

(erc-autojoin
 (("irc.freenode.net") "#marksy" "#raptracks" "#clojure" "#paperswelove"))

(defun kyle-connect ()
  (interactive)
  (erc-tls :server "irc.tyrfingr.is" :port 6697 :nick "zeeshanlakhani" :password nil)
  (erc-join-channel "#tyrfingr"))

;; switch windows with fun
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-l") 'goto-line)

;; handle for paredit
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

;; Fix for shift up = <select> is undefined
(define-key input-decode-map "\e[1;2A" [S-up])

(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; size buffers
(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

;; find func
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "M-z") 'ff-find-other-file)

;; helm
(global-set-key (kbd "C-x C-d") 'helm-mini)

;; yasnippet
(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

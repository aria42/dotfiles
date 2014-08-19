(defun theme-dark ()
  (interactive)
  (load-theme 'zenburn t)
  (set-face-background 'default "#222")
  (set-face-background 'region "#374186")
  (set-face-background 'fringe "#191919")
  (set-face-background 'hl-line "#191919"))

(theme-dark)

;; fonts

(setq default-frame-alist '((font . "Ubuntu Mono for Powerline-16")))

(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "#202020")
     (set-face-foreground 'magit-diff-add "#40ff40")
     (set-face-foreground 'magit-diff-del "#ff4040")
     (set-face-foreground 'magit-diff-file-header "#4040ff")))

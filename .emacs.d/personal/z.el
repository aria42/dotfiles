;; no tabs, normally
(setq-default tab-width 4)
(setq c-basic-offset 4)

;;;; for java-mode
(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   indent-tabs-mode nil)))

;;;;finally
(setq-default indent-tabs-mode nil)

;; arrows come back
(setq prelude-guru nil)

;; whitespace issues prelude
(setq prelude-whitespace nil)

;; no hard tabs, use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Disable all the version control stuff
;; Makes emacs load much faster inside git repos
(setq vc-handled-backends nil)

;; switch windows with fun
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-l") 'goto-line)

;; dirtree
(require 'dirtree)
(global-set-key (kbd "C-x C-z") 'dirtree)

;; autocomplete
(load-file "~/.emacs.d/vendor/autocomplete/auto-complete.el")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 2)

;; geiser
(load-file "~/.emacs.d/vendor/geiser/elisp/geiser.el")

;; quack
(load-file "~/.emacs.d/vendor/quack.el")

;; anything
(require 'anything-config)
(global-set-key (kbd "C-x C-d") 'anything)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/personal/snippets"
        "~/.emacs.d/vendor/yasnippet/extras/imported"
        ))
(yas-global-mode 1)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

;; b/c of autocomplete/yas-snippet issues

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 120)

(setq whitespace-style '(face tabs tab-mark lines-tail trailing))
(set-face-foreground 'whitespace-tab nil)
(set-face-background 'whitespace-tab nil)

(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; modeline
(require 'modeline-posn)
(setq modelinepos-column-limit 80)
(size-indication-mode 1)

;; other file loaders
(add-to-list 'load-path "~/.emacs.d/personal/files")

;; compile all the files .elc files which has a corresponding newer .el file
(byte-recompile-directory "~/.emacs.d/core" "~/.emacs.d/modules")
;; like before, but in this case force the byte-compilation of an .el file when the corresponding
;; .elc file doesn't exist
(byte-recompile-directory "~/.emacs.d/core" "~/.emacs.d/modules" 0)

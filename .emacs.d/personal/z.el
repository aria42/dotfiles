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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/dict")
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start t)

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
(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

;; whitespace
(require 'whitespace)
 (setq whitespace-line-column 120)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

;; other file loaders
(add-to-list 'load-path "~/.emacs.d/personal/files")

;; compile all the files .elc files which has a corresponding newer .el file
(byte-recompile-directory "~/.emacs.d/core" "~/.emacs.d/modules")
;; like before, but in this case force the byte-compilation of an .el file when the corresponding
;; .elc file doesn't exist
(byte-recompile-directory "~/.emacs.d/core" "~/.emacs.d/modules" 0)

;; no tabs, normally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)

;; arrows come back
(setq prelude-guru nil)

;; whitespace issues prelude
(setq prelude-whitespace nil)

;; turn off prelude tip of the day
(setq prelude-tip-of-the-day nil)

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

;; find func
(global-set-key (kbd "C-h C-f") 'find-function)

;; dirtree
(autoload 'dirtree "dirtree" "dirtree" t)
(global-set-key (kbd "C-x C-z") 'dirtree)

;; autocomplete
(load-file "~/.emacs.d/vendor/autocomplete/auto-complete.el")
(autoload 'auto-complete-config "auto-complete-config" "autocomplete" t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 2)

;; geiser, ecd, palm for racket/scheme
(load-file "~/.emacs.d/vendor/geiser/elisp/geiser.el")
(setq geiser-active-implementations '(racket))
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; anything
(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "C-x C-d") 'anything)

;; yasnippet
(autoload 'yasnippet "yasnippet" "yasnippet" t)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/personal/snippets"
        "~/.emacs.d/vendor/yasnippet/extras/imported"
        ))
(yas-global-mode 1)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

;; whitespace
(autoload 'whitespace "whitespace" "whitespace" t)
(setq whitespace-line-column 120)

(setq whitespace-style '(face lines-tail trailing))
(set-face-foreground 'whitespace-tab nil)
(set-face-background 'whitespace-tab nil)

(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; modeline
(autoload 'modeline-posn "modeline-posn" "modeline-posn" t)
(setq modelinepos-column-limit 80)
(size-indication-mode 1)

;; markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
'(markdown-enable-math t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

;; json-mode
(load-file "~/.emacs.d/vendor/json-mode/json-mode.el")
(autoload 'json-mode "json-mode" "Major mode for editing Json files")

;; js2-mode (installed via package manager)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; other file loaders
(add-to-list 'load-path "~/.emacs.d/personal/files")

;; load lang-specific hooks
(load-file "~/.emacs.d/personal/langs.el")

;; compile all the files .elc files which has a corresponding newer .el file, if it exists
(byte-recompile-directory "~/.emacs.d/core" "~/.emacs.d/modules" 0)

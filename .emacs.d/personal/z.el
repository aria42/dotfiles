;; packages that must be installed via package-manager
(prelude-ensure-module-deps '(ac-js2 ac-math ac-nrepl ace-jump-mode
                                     ack-and-a-half android-mode cl-lib clojure-mode
                                     clojure-test-mode color-theme color-theme-solarized
                                     dash diminish dummy-h-mode erc-hl-nicks erlang ctags
                                     exec-path-from-shell expand-region flymake-easy ctags-update
                                     flymake-haskell-multi flymake-json flyspell-lazy sml-mode
                                     gh gist git-commit-mode gitconfig-mode gitignore-mode
                                     go-autocomplete go-mode guru-mode haml-mode haskell-mode
                                     helm helm-projectile js2-mode logito lua-mode magit
                                     melpa nginx-mode nrepl org paredit pcache popup
                                     projectile rainbow-delimiters rainbow-mode sass-mode
                                     scss-mode simple-httpd skewer-mode spotify tree-mode
                                     undo-tree volatile-highlights icicles
                                     processing-mode zenburn-theme))

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

;; Show line-number on left-hand side
(global-linum-mode 0)
(add-hook 'linum-before-numbering-hook
          (lambda () (setq linum-format "%d ")))

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; disable menu at top
(menu-bar-mode -99)

;; Disable all the version control stuff
;; Makes emacs load much faster inside git repos
(setq vc-handled-backends nil)

;; change-up chord mode
(key-chord-define-global "jj" nil)
(key-chord-define-global "uu" nil)

;; switch windows with fun
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-l") 'goto-line)

;; Fix for shift up = <select> is undefined
(define-key input-decode-map "\e[1;2A" [S-up])

(if (equal "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up]))

(defadvice terminal-init-xterm (after select-shift-up activate)
    (define-key input-decode-map "\e[1;2A" [S-up]))

;; have emacs prompt short-handed y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ignore byte-compile warnings
(setq byte-compile-warnings nil)

;; size buffers
(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

;; find func
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "M-z") 'ff-find-other-file)

;; Set *scratch* empty
(setq initial-scratch-message nil)

;; Set *scratch* to Clojure mode
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; set autocorrect-spelling
(setq ispell-program-name "/usr/local/bin/aspell")

;; turn icicles on
(icy-mode 1)

;; dirtree
(autoload 'dirtree "dirtree" "dirtree" t)
(global-set-key (kbd "C-x C-z") 'dirtree)

;; autocomplete
(load-file "~/.emacs.d/vendor/autocomplete/auto-complete.el")
(load-file "~/.emacs.d/vendor/auto-complete-clang/auto-complete-clang.el")

(autoload 'auto-complete-config "auto-complete-config" "autocomplete" t)
(require 'auto-complete-clang-async)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/dict")

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/vendor/emacs-clang-complete-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(setq-default ac-sources (add-to-list
                          'ac-sources 'ac-source-dictionary
                          'ac-source-words-in-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                    sass-mode scss-mode yaml-mode csv-mode espresso-mode
                                    haskell-mode html-mode nxml-mode sh-mode smarty-mode
                                    clojure-mode lisp-mode textile-mode markdown-mode
                                    tuareg-mode js3-mode js2-mode css-mode less-css-mode
                                    objc-mode sql-mode processing-mode))
  (add-to-list 'ac-modes mode))

(add-hook 'objc-mode-hook 'ac-cc-mode-setup)

(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

(global-set-key (kbd "C-`") 'ac-complete-clang)

;; yasnippet
(load-file "~/.emacs.d/vendor/yasnippet/yasnippet.el")
(autoload 'yasnippet "yasnippet" "yasnippet" t)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/personal/snippets"
        "~/.emacs.d/vendor/yasnippet/extras/imported"
        ))
(add-to-list 'ac-sources 'ac-source-yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

;; yasnippet -> add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

(setq yas-prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

;; geiser, ecd, palm for racket/scheme
(load-file "~/.emacs.d/vendor/geiser/elisp/geiser.el")
(setq geiser-active-implementations '(racket))
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; ack-and-a-half
(autoload 'ack-and-a-half "ack-and-a-half" "ack-and-a-half" t)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; helm shorcut
(global-set-key (kbd "C-x C-d") 'helm-mini)

;; surpress killing confirms
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; hide certain compilation buffers
(require 'aj-compilation)

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

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; pod files
(add-to-list 'auto-mode-alist '("Podfile$" . ruby-mode)) ;; support Podfiles
(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode)) ;; support Podspecs

;; rubyisms
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; processing
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/usr/bin/processing-java")
(setq processing-application-dir "/Applications/Processing.app")
(setq processing-sketch-dir "~/Documents/Processing")

;; other file-exts for clojure-mode
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

;; clojure cheatsheet
(load-file "~/.emacs.d/vendor/clojure-cheatsheet/clojure-cheatsheet.el")

;; nREPL customizations

; Don't aggressively popup stacktraces
(setq nrepl-popup-stacktraces nil)
; Display stacktrace inline
; (setq nrepl-popup-stacktraces-in-repl t)
; Enable eldoc - shows fn argument list in echo area
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
; Use paredit in *nrepl* buffer
(add-hook 'nrepl-mode-hook 'paredit-mode)
; Make C-c C-z switch to *nrepl*
(add-to-list 'same-window-buffer-names "*nrepl*")
; enable camelcase for editing commands
(add-hook 'nrepl-mode-hook 'subword-mode)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; load lang-specific hooks
(load-file "~/.emacs.d/personal/langs.el")
(load-file "~/.emacs.d/personal/erc/erc-fun.el")
(load-file "~/.emacs.d/personal/theme-ing.el")
(load-file "~/.emacs.d/personal/fonts.el")
(load-file "~/.emacs.d/personal/objective-c-customizations.el")

;;;; other file loaders
(add-to-list 'load-path "~/.emacs.d/personal/files")

;; compile all the files .elc files which has a corresponding newer .el file, if it exists
(byte-recompile-directory "~/.emacs.d" 0 nil)

;; remove .elc on save hook
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

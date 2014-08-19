(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; add marmelade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; no tabs, normally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)

;; arrows come back
(setq prelude-guru nil)

;; special mac-ness
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; eletric indentation
(electric-indent-mode +1)

;; whitespace issues prelude
(setq prelude-whitespace nil)

;; turn off prelude tip of the day
(setq prelude-tip-of-the-day nil)

;; Show line-number on left-hand side
(global-linum-mode 1)
(add-hook 'linum-before-numbering-hook
          (lambda () (setq linum-format "%d ")))

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; disable menu,scroll,tool at top
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable all the version control stuff
;; Makes emacs load much faster inside git repos
(setq vc-handled-backends nil)

;; make emacs pretty
(global-pretty-mode t)

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

;; mathiness

(autoload 'tex-math-preview "tex-math-preview" nil t)

;; hidden-mode-line

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
           hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

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
                                    objc-mode sql-mode processing-mode scheme-mode))
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
;;(load-file "~/.emacs.d/vendor/geiser/elisp/geiser.el")
;;(setq geiser-active-implementations '(racket))
;;(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

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
(setq whitespace-line-column 80)

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

;; js2-mode w/refactor
(js2r-add-keybindings-with-prefix "C-c C-d")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

;; clojure refactor
(cljr-add-keybindings-with-prefix "C-c C-g")

;; scheme - iuscheme - petite chez
(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
(add-to-list 'auto-mode-alist
             '("\\.ss" . scheme-mode)
             '("\\.scm" . scheme-mode))

(custom-set-variables '(scheme-program-name "petite"))

(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)

;; nREPL/cider customizations

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "C-S-<left>") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-S-<right>") 'paredit-backward-barf-sexp)

(add-hook 'clojure-mode-hook          'paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'paredit-mode)
(add-hook 'lisp-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook           'paredit-mode)

(define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
(define-key clojure-mode-map (kbd "C-o J") 'cider-restart)

; hide extra buffers
(setq cider-hide-special-buffers t)
; enable eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
; prevent auto-display of repl buffer
(setq cider-repl-pop-to-buffer-on-connect nil)
; Don't aggressively popup stacktraces
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
; Display stacktrace inline
; (setq cider-popup-stacktraces-in-repl t)
; Use paredit in *nrepl* buffer
(add-hook 'cider-repl-mode-hook 'paredit-mode)
; enable camelcase for editing commands
(add-hook 'cider-repl-mode-hook 'subword-mode)
; rainbow-delim
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
; c-c c-z swith to current window
(add-to-list 'same-window-buffer-names "*cider*")
; autoselect error buffer
(setq cider-auto-select-error-buffer t)
(setq cider-stacktrace-fill-column 80)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-use-clojure-font-lock t)

(add-hook 'after-init-hook 'global-company-mode)

;; org-mode the things

(defun org-insert-scaled-screenshot ()
  (interactive)
  (let ((filename
         (concat "screenshot-"
                 (substring
                  (shell-command-to-string
                   "date +%Y%m%d%H%M%S")
                  0 -1)
                 ".png")))
    (let ((scaledname
           (concat filename "-width300.png")))

      (shell-command
       (concat "import -window root "
               filename))
      (shell-command
       (concat "convert -adaptive-resize 300 "
               filename " " scaledname))
      (insert (concat "[[./" scaledname "]]")))))

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))

(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

;; We only need Emacs Lisp and Clojure in this tutorial:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))

;; Use cider as the clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-replace-disputed-keys t
      org-support-shift-select t)

;; Useful keybindings when using Clojure from Org
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

(require 'org-present)

(require 'ac-cider)
(require 'ac-cider-compliment)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-compliment-repl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; load lang-specific hooks
(load-file "~/.emacs.d/personal/langs.el")
(load-file "~/.emacs.d/personal/erc/erc-fun.el")
(load-file "~/.emacs.d/personal/theme-ing.el")
(load-file "~/.emacs.d/personal/fonts.el")
(load-file "~/.emacs.d/personal/objective-c-customizations.el")

;; special newline madness
(setq require-final-newline t)

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

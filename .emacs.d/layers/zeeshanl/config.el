;; no tabs, normally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)

;; javascript/web indent

(push "~/.emacs.d/local/prettier/editors/emacs/" load-path)
(require 'prettier-js)

(defun setup-indent (n)
  (setq-default coffee-tab-width n)       ; coffeescript
  (setq-default javascript-indent-level n) ; javascript-mode
  (setq-default js-indent-level n) ; js-mode
  (setq-default js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-default css-indent-offset n) ; css-mode
  )
(setup-indent 2)

;; eletric indentation and RET
(electric-indent-mode +1)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Show line-number on left-hand side
(global-linum-mode 1)
(add-hook 'linum-before-numbering-hook
          (lambda () (setq linum-format "%d ")
            (set-face-attribute 'linum nil :height 80)))


;; Disable all the version control stuff
;; Makes emacs load much faster inside git repos
(setq vc-handled-backends nil)

;; have emacs prompt short-handed y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ignore byte-compile warnings
(setq byte-compile-warnings nil)

;; Set *scratch* empty
(setq initial-scratch-message nil)

;; surpress killing confirms
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

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

;; nREPL/cider customizations

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

;; opamz
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

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

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

;; ansi-term
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; special newline madness
(setq require-final-newline t)

;; whitespace
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
(setq whitespace-line-column 80)

(setq whitespace-style '(face lines-tail trailing))

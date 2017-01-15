;; Erlang Emacs Mode -- Configuration Start

(eval-when-compile (require 'cl))

;; Add files to erlang-mode
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode)) ;; default .erl
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode)) ;; rebar
(add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode)) ;; rebar
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode)) ;; embedded node/riak
(add-to-list 'auto-mode-alist '(".riak_test.config" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.app\\.src" . erlang-mode))

;; We never want to edit Erlang .beam files
(add-to-list 'completion-ignored-extensions ".beam")

(setq erlang-flymake-command "/usr/local/lib/erlang/bin/erlc")

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9/emacs" load-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

(add-hook 'erlang-mode-hook 'whitespace-mode)

; add include directory to default compile path.
(setq erlang-compile-extra-opts
      '(bin_opt_info debug_info (i . "../../../include") (i . "../include")
                     (i . "include") (i . "../deps") (i . "../../")
                     (i . "../../../deps")))

(setq erlang-electric-arrow-criteria (list (lambda () 'stop)))

;; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;;;----------------------------------------
;;; distel -> brought in from wrangler
;;;----------------------------------------

;;(push "~/.emacs.d/local/distel/elisp/" load-path)
;;(require 'distel)
;;(distel-setup)

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)
;; default node name to emacs@<hostname>
(setq inferior-erlang-machine-options '("-sname" "emacs"))
;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol "emacs@localhost"
       ;; (concat
       ;;  "emacs@"
       ;;  ;; Mac OS X uses "name.local" instead of "name", this should work
       ;;  ;; pretty much anywhere without having to muck with NetInfo
       ;;  ;; ... but I only tested it on Mac OS X.
       ;;  (car (split-string (shell-command-to-string "hostname"))))
       ))

;;;----------------------------------------
;;; flymake
;;;----------------------------------------

(require 'flymake)
(require 'erlang-flymake)

(setq flymake-log-level 3)
(defun flymake-compile-script-path (path)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

(defun flymake-syntaxerl ()
  (flymake-compile-script-path "~/.emacs.d/local/erlang/flymake-compile-erlang"))

(add-hook 'erlang-mode-hook
          '(lambda()
             (flycheck-mode 0)
             (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))
             (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))
             ;; should be the last.
             (flymake-mode 1)))

(erlang-flymake-only-on-save)

(defun pretty-erlang ()
  (interactive)
  (let ((replacements '(("\\(->\\)"             . "→")
                        ("\\(<-\\)"             . "←")
                        ("\\(||\\)"            . "‖")
                        ;("[^=]\\(==\\)[^=]"     . "≈")
                        ;("\\(=:=\\)"            . "≡")
                        ;("\\(=/=\\)"            . "≢")
                        ;("\\(/=\\)"             . "≠")
                        ("\\(=<\\)"             . "≤")
                        ("\\(>=\\)"             . "≥")
                        ;("\\(<<\\)"             . "«")
                        ;("\\(>>\\)"             . "»")
                        ;("\\(*\\)"              . "×")
                        ;("\\(::\\)"            . "⁛")
                        ;("\\(~s\\)"            . "Ⓢ")
                        ;("\\(~p\\)"            . "Ⓟ")
                        ;("\\(~w\\)"            . "Ⓦ")
                        ;("\\(~n\\)"             . "↵")
                        ;("\\(\\<fun\\>\\)[( ]" . "λ")
                        ;("\\(\\<catch\\>\\) "  . "☂")
                        )))
    (font-lock-add-keywords
     nil
     (mapcar
      (lambda (pair)
        `(,(car pair)
          (0 (prog1 nil
               (compose-region (match-beginning 0) (match-end 0) ,(cdr pair))))))
      replacements))))
(add-hook 'erlang-mode-hook 'pretty-erlang)

;;;
;; Paredit Things
;;;

;; define paredit mode-keys
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [?\{] 'paredit-open-curly)
     (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
     (define-key paredit-mode-map [?\}] 'paredit-close-curly)
     (define-key paredit-mode-map [?\]] 'paredit-close-square)
;;   (define-key paredit-mode-map [?\>] 'paredit-close-angled)
     (define-key paredit-mode-map [(meta ?\))]
                 'paredit-close-parenthesis-and-newline)))

(defvar other-paredit-modes
  '(erlang-mode erlang-extended-mode erlang-shell-mode)
  "Non-lisp modes for which paredit is still useful.")

(defun pnh-paredit-no-space ()
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil))))

;; hooks
(add-hook 'erlang-mode-hook 'auto-complete-mode)
(add-hook 'erlang-mode-hook 'pnh-paredit-no-space)
(add-hook 'erlang-shell-mode-hook 'auto-complete-mode)
(add-hook 'erlang-shell-mode-hook 'pnh-paredit-no-space)

(add-hook 'elixir-mode-hook 'pnh-paredit-no-space)
(add-hook 'alchemist-mode-hook 'pnh-paredit-no-space)
(add-hook 'alchemist-iex-mode-hook 'pnh-paredit-no-space)

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(add-hook 'erlang-new-file-hook 'tempo-template-erlang-large-header)

;;;----------------------------------------
;;; wrangler
;;;----------------------------------------
(add-to-list 'load-path "~/erlang/lib/wrangler/elisp")
(require 'wrangler)
(load-library "graphviz-dot-mode")

;; Erlang Emacs Mode -- Configuration End

;; EQC Emacs Mode -- Configuration Start
(add-to-list 'load-path "~/erlang/lib/eqc/eqc-1.39.2/emacs")
(autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
(add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
(setq eqc-max-menu-length 30)
(setq eqc-root-dir "~/erlang/lib/eqc/eqc-1.39.2")
;; EQC Emacs Mode -- Configuration End

(setq zeeshanl-packages '((pretty-mode :location elpa)
                          (paredit :location elpa)))

(defun zeeshanl/init-pretty-mode ()
  (use-package pretty-mode
    :defer t
    :config (message "Loaded pretty-mode")))

(defun zeeshanl/init-paredit ()
  (use-package paredit
    :defer t
    :config (message "Loaded paredit-mode")
    '(progn
       (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
       (define-key paredit-mode-map (kbd "M-(")) 'paredit-wrap-round)
       (define-key paredit-mode-map (kbd "C-S-<left>")
         'paredit-backward-slurp-sexp)
       (define-key paredit-mode-map (kbd "C-S-<right>")
         'paredit-backward-barf-sexp)))

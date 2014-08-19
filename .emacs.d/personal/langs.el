;; explicit for modes

(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   indent-tabs-mode nil)))

(add-hook 'json-mode-hook
          '(lambda ()
             (setq js-indent-level 2)))

(setq js2-mode-hook
      '(lambda () ((progn)
                (setq js2-missing-semi-one-line-override t)
                (set-variable 'indent-tabs-mode nil))))

(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))

(defun processing-mode-init ()
  (autoload 'processing-snippets-initialize "processing-mode" nil nil nil)
  (eval-after-load 'yasnippet '(processing-snippets-initialize))

  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary processing-functions)
  (setq ac-user-dictionary (append ac-user-dictionary processing-builtins))
  (setq ac-user-dictionary (append ac-user-dictionary processing-constants)))

(add-hook 'processing-mode-hook 'processing-mode-init)

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               (cljr-add-keybindings-with-prefix "C-c C-f")))

(add-hook 'scheme-mode-hook 'balanced-on)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

;;;; explicit for modes

(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   indent-tabs-mode nil)))

(add-hook 'json-mode-hook
          '(lambda ()
             (setq js-indent-level 2)))

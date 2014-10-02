(setq z-erlang-packages '((flycheck-tip :location elpa)
                          (distel :location local)
                          (company-distel :location elpa)))

(defun z-erlang/init-flycheck-tip ()
  (use-package flycheck-tip
    :defer t
    :config (message "Loaded Flycheck tip")))

(defun z-erlang/init-distel ()
  (use-package distel
    :defer t
    :config (message "Loaded Distel")))

(defun z-erlang/init-company-distel ()
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-distel))

  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq company-backends '(company-distel)))))


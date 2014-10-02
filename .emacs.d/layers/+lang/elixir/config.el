;;; config.el --- Elixir Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar elixir-enable-compilation-checking nil
  "If non-nil syntax checking is enable for compilation.
Default is nil because Elixir compilation is based on macros and thus it
is unsafe. Activate this option only for trusted code, usage of a
directory variable is recommended.")

(spacemacs|defvar-company-backends elixir-mode)
(spacemacs|defvar-company-backends alchemist-iex-mode)

(spacemacs|define-jump-handlers elixir-mode)

;; handle alchemist issues around umbrella projects

(defadvice alchemist-project-root (around zl/alchemist-project-root activate)
  (let ((alchemist-project-mix-project-indicator ".git"))
    ad-do-it))

(defun zl/activate-alchemist-root-advice ()
  "Activates advice to override alchemist's root-finding logic"
  (ad-activate 'alchemist-project-root))

(add-to-list 'elixir-mode-hook 'zl/activate-alchemist-root-advice)

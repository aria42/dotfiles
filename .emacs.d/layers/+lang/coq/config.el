;;; config.el --- Coq Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Olivier Verdier <olivier.verdier@gmail.com>
;; URL: https://github.com/olivierverdier/spacemacs-coq
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar proof-general-path "~/.emacs.d/local/proof-general/generic/proof-site"
  "The path to proof general")

;; coq mode for .v files
(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))



;;; taken from https://github.com/GriffinSchneider/emacs-config

(defvar anything-c-source-objc-headline
  '((name . "Objective-C Headline")
    (headline  "^[-+@]\\|^#pragma mark")))

(defun objc-headline ()
  (interactive)
  ;; Set to 500 so it is displayed even if all methods are not narrowed down.
  (let ((anything-candidate-number-limit 500))
    (anything-other-buffer '(anything-c-source-objc-headline)
                           "*ObjC Headline*")))

;; Use objc-mode for objective-c++ files
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.pch$" . objc-mode))

;; Auto mode
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

(defun gcs-objc-mode-hook ()
  ;; Make ff-find-other-file toggle between .m and .h
  (set (make-local-variable 'cc-other-file-alist)
       '(("\\.m" (".h")) ("\\.h" (".m"))))

  (local-set-key (kbd "C-x p") 'objc-headline)

  ;; autcomplete
  (setq ac-modes (append ac-modes '(objc-mode)))
  (lambda () (setq ac-sources (append '(ac-source-clang
                                        ac-source-yasnippet
                                        ac-source-gtags)
                                      ac-sources)))
  ;; Setup indentation
  (setq tab-width 4)
  (c-set-style "java")
  (c-set-offset 'brace-list-close '-)
  (c-set-offset 'brace-list-intro '0)
  (c-set-offset 'arglist-close '0)
  ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'brace-list-open '+)
  ; indent case labels by c-indent-level, too
  (c-set-offset 'case-label '+))
(add-hook 'objc-mode-hook 'gcs-objc-mode-hook)

;; elisp throws an error here, but we gots it loaded ok
(provide 'objective-c-customizations)

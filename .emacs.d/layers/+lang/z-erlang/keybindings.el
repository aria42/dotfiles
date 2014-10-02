(defun erlang-insert-binary ()
  "Inserts a binary string into an Erlang buffer and places the
  point between the quotes."
  (interactive)
  (insert "<<\"\">>")
  (backward-char 3)
  )

(eval-after-load "erlang" '(define-key erlang-mode-map (kbd "C-c b")
                             'erlang-insert-binary))
(eval-after-load "alchemist" '(define-key alchemist-mode-map (kbd "C-c b")
                                'erlang-insert-binary))


(defun anything-but-gnu-indents-please ()
  (define-key c-mode-map (kbd "<return>") 'newline-and-indent)
  (c-set-style "linux"))

(add-hook 'c-mode-hook 'anything-but-gnu-indents-please)

(provide 'starter-kit-c)
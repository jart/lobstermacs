;;; lobstermacs-go.el
;;
;; Customizations for Google's Go Programming Language
;;

(defun lob/go-mode-hook ()
  (make-local-variable 'tab-width)
  (setq-default tab-width 4))

(autoload (quote go-mode) "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (cons "\\.go$" (function go-mode)))

(eval-after-load 'go-mode
  '(progn
     (define-key go-mode-map (kbd "RET") 'newline-and-indent)
     (define-key go-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key go-mode-map (kbd "C-<return>") 'c-indent-new-comment-line)
     (add-hook 'go-mode-hook 'lob/go-mode-hook)
     ))

(provide 'lobstermacs-go)

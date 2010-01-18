;;; lobstermacs-lisp.el --- Define some custom functions
;;
;; Part of Lobstermacs

(eval-after-load 'lisp-mode
  '(progn
     (add-hook 'emacs-lisp-mode-hook 'turn-on-company)))

;; paredit tames parenthesis.  never code lisp without it <3
(eval-after-load 'paredit
  '(progn
     ;; these bindings make paredit easier to use
     (define-key paredit-mode-map (kbd "C-<return>") 'paredit-close-parenthesis-and-newline-safe)
     (define-key paredit-mode-map (kbd "C-c C-s") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c C-b") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c C-r") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "C-c C-j") 'paredit-join-sexps)

     ;; these bindings make paredit feel less buggy
     (define-key paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "<DEL>") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis-safe)))

(defun paredit-close-parenthesis-safe ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis)
                  (insert ")")))

(defun paredit-close-parenthesis-and-newline-safe ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis-and-newline)
                  (insert ")")))

(provide 'lobstermacs-lisp)
;;; lobstermacs-lisp.el ends here

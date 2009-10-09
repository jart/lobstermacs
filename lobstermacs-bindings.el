;;; lobstermacs-bindings.el --- My special keybindings
;;
;; Part of Lobstermacs.

;; auto modes for certain extensions
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.\(net|com|org|info|us\).db$" . zone-mode))
(add-to-list 'auto-mode-alist '("bind/db\\." . zone-mode))
(add-to-list 'auto-mode-alist '("named/db\\." . zone-mode))

;; Additional Paredit bindings
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "<DEL>") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd ")")   'paredit-close-parenthesis-safe)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-close-parenthesis-and-newline-safe)
     (define-key paredit-mode-map (kbd "C-c C-s") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c C-b") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c C-r") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-c C-s") 'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "C-c C-j") 'paredit-join-sexps)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)))

;; If we're typing on a region and press backspace, the region should die.
(global-set-key (kbd "C-d") 'delete-char-dwim)
(global-set-key (kbd "<DEL>") 'delete-backward-char-dwim)

;; These key-bindings make it very easy to navigate through grep
;; results, compile errors, etc.
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)

;; Some Mac-friendly key counterparts
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)

;; function keyz
(global-set-key [f3] 'sudo-edit-me)
(global-set-key [f11] 'next-error)
(global-set-key [f12] 'previous-error)

(provide 'lobstermacs-bindings)
;;; lobstermacs-bindings.el ends here

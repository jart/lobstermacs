;;; lobstermacs-bindings.el --- My special keybindings
;;
;; Part of Lobstermacs.

;; an oldy but a goody.  this makes the cursor stay in the same page
;; on the screen when you use `C-v` and `M-v` so you don't have to
;; keep re-centering with `C-l`
(require 'pager)
(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key (kbd "M-v") 'pager-page-up)

;; make emacs slightly less astonishing for windows users without
;; provoking the wrath of emacs aficionados.  Cua-Lite is configured
;; here to only really kick in if you use the arrow keys, page
;; up/down, etc.
;;
;; You can now highlight with the shift and arrow keys like everyone
;; else in the world.  Now I just need to figure out how to make
;; `C-c`, `C-x` and `C-v` behave the Windows way when a selection was
;; created using the mouse or arrow keys.
(require 'cua-lite)
(setq cua-lite-default-keybindings 1
      cua-lite-mode-line-string ""
      cua-lite-use-backward-delete-word nil
      cua-lite-use-simplified-paragraph-movement t
      cua-lite-what-is-alt-f4 nil
      cua-lite-what-is-control-w nil
      cua-lite-display-status-in-mode-line nil
      cua-lite-use-hscroll-mode nil)
(cua-lite 1)

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
     (define-key paredit-mode-map (kbd "C-c C-l") 'paredit-split-sexp)
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

;; being able to minimize emacs isn't very helpful, especially if it
;; makes life harder for windows ppl
(if window-system (global-set-key (kbd "C-z") 'undo))

;; function keyz
(global-set-key [f3] 'sudo-edit-me)
(global-set-key [f11] 'next-error)
(global-set-key [f12] 'previous-error)

;; IDO execute is cool
(global-set-key (kbd "M-x") 'ido-execute-command)
(global-set-key (kbd "C-x C-m") 'ido-execute-command)

;; opposite of `M-q`
(global-set-key (kbd "M-Q") 'unfill-paragraph)

(provide 'lobstermacs-bindings)
;;; lobstermacs-bindings.el ends here

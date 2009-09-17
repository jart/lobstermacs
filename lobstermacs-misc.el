;;; lobstermacs-misc.el --- Random stuff
;;
;; Part of Lobstermacs.

;; Lots here stolen from topfunky

(server-start)
(prefer-coding-system 'utf-8)

;; Avoid error: "Variable binding depth exceeds max-specpdl-size"
(setq max-specpdl-size 32000
      column-number-mode 1
      make-backup-files nil)

;; Overwrite highlighted text if you start typing
(delete-selection-mode t)

;; auto modes for certain extensions
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; ReST files from bitbucket in particular have the header
;; ".. -*-restructuredtext-*-" which emacs isn't going to understand
;; without this alias
(defalias 'restructuredtext-mode 'rst-mode)

;; Additional Paredit bindings
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "<DEL>") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd ")")   'paredit-close-parenthesis-safe)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-close-parenthesis-and-newline-safe)
     (define-key paredit-mode-map (kbd "M-s") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-v") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)))

;; If we're typing on a region and press backspace, the region should die.
(global-set-key (kbd "C-d") 'delete-char-dwim)
(global-set-key (kbd "<DEL>") 'delete-backward-char-dwim)

;; These key-bindings make it very easy to navigate through grep
;; results, compile errors, etc.
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)
(global-set-key [f11] 'next-error)
(global-set-key [f12] 'previous-error)

;; Some Mac-friendly key counterparts
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)

;; Save backups in one place Put autosave files (ie #foo#) in one
;; place, *not* scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  "Override default emacs behavior"
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Plain Text
;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(provide 'lobstermacs-misc)
;;; lobstermacs-misc.el ends here
;;; lobstermacs-misc.el --- Random stuff
;;
;; Part of Lobstermacs.

(prefer-coding-system 'utf-8)

;; lets you use the mouse in terminal mode.  seems to be a bug using
;; this on read-only buffers.  also highlighting doesn't take effect
;; until you let go of the mouse :\  ALSO this highlighting doesn't
;; seem to work when editing lisp files
(when (not window-system)
  (xterm-mouse-mode t))

;; Line numbers are cool but you can always have too much of a good
;; thing.  Disable `linum-on` so it doesn't apply to popup buffers
;; starting with '*' like `*Ido Completions*` and `*Help*`.  Disabled
;; when running from terminal, looks kinda bad
(defun linum-on ()
    (unless (or (minibufferp)
                (string-match "^\*" (buffer-name (current-buffer))))
      (linum-mode 1)))
(if (and (fboundp 'global-linum-mode)
         (not window-system))
    (custom-set-faces
     '(linum ((t (:foreground "brightblack")))))
    (global-linum-mode 1))

;; tiny scroll bars in minibuffer is silly
(set-window-scroll-bars (minibuffer-window) nil)

;; start emacs server, but not if some other emacs instance already
;; has a server
(if (fboundp 'server-start)
    (progn
      (require 'server) ;; server-running-p is not autoloaded
      (if (not (server-running-p))
          (server-start))))

;; Avoid error: "Variable binding depth exceeds max-specpdl-size"
(setq max-specpdl-size 32000
      column-number-mode 1
      make-backup-files nil)

;; Overwrite highlighted text if you start typing
(delete-selection-mode t)

;; what the heck is `default-push`?
(setq hg-outgoing-repository "default")

;; ReST files from bitbucket in particular have the header
;; ".. -*-restructuredtext-*-" which emacs isn't going to understand
;; without this alias
(defalias 'restructuredtext-mode 'rst-mode)

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

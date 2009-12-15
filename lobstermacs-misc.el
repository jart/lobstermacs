;;; lobstermacs-misc.el --- Random stuff
;;
;; Part of Lobstermacs.

(prefer-coding-system 'utf-8)

(setq max-specpdl-size 32000 ;; Avoid error: "Variable binding depth exceeds max-specpdl-size"
      column-number-mode 1
      make-backup-files nil
      ourcomments-ido-ctrl-tab t
      ourcomments-M-x-menu-mode t
      require-final-newline t
      hg-outgoing-repository "default")

;; lets you use the mouse in terminal mode.  seems to be a bug using
;; this on read-only buffers.  also highlighting doesn't take effect
;; until you let go of the mouse :\  ALSO this highlighting doesn't
;; seem to work when editing lisp files.
;;
;; I'm keeping this off because most people expect to be able to use
;; the *terminal* itself to copy/paste
;; (when (not window-system)
;;   (xterm-mouse-mode t))

;; Line numbers are cool but you can always have too much of a good
;; thing.  Disable `linum-on` so it doesn't apply to popup buffers
;; starting with '*' like `*Ido Completions*` and `*Help*`.  We're not
;; going to bother with this on 4-bit terminals.
;;
;; BUG: flaky with company completion popups
(when (and (fboundp 'global-linum-mode)
           lob/is-colorful)
  (global-linum-mode 1)
  ;; in terminal there's no gutter, so give a little space
  (if (not window-system)
      (setq linum-format 'lob/linum-format-with-space)))
(defun linum-on ()
  (unless (or (minibufferp)
              (string-match "^\*" (buffer-name (current-buffer))))
    (linum-mode 1)))

;; tiny scroll bars in minibuffer is silly
(set-window-scroll-bars (minibuffer-window) nil)

;; start emacs server, but not if some other emacs instance already
;; has a server
(if (fboundp 'server-start)
    (progn
      (require 'server) ;; server-running-p is not autoloaded
      (if (not (server-running-p))
          (server-start))))

;; Overwrite highlighted text if you start typing
(delete-selection-mode t)

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

;; trying to make shell suck less
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'lobstermacs-misc)
;;; lobstermacs-misc.el ends here

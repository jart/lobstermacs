;;; lobstermacs-misc.el --- Random stuff
;;
;; Part of Lobstermacs.

(setq max-specpdl-size 32000 ;; Avoid error: "Variable binding depth exceeds max-specpdl-size"
      column-number-mode 1
      make-backup-files nil
      ourcomments-ido-ctrl-tab t
      ourcomments-M-x-menu-mode t
      require-final-newline t
      hg-outgoing-repository "default")

;; View python eggs as compressed archives
(auto-compression-mode t)
(if (not (member ".egg" tags-compression-info-list))
    (setq tags-compression-info-list
          (cons ".egg" tags-compression-info-list)))

;; Make emacs friendlier to Mac users
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)

;; Make emacs slightly friendlier to windows users.  Right now we
;; enable cua-lite purely to allow shift+arrow highlighting.
;;
;; ToDo: Make `C-c`, `C-x` and `C-v` do copy/pasting *only* there is a
;;region something was highlighted using the mouse or shift+arrows.
(setq cua-lite-default-keybindings 1
      cua-lite-mode-line-string ""
      cua-lite-use-backward-delete-word nil
      cua-lite-use-simplified-paragraph-movement t
      cua-lite-what-is-alt-f4 nil
      cua-lite-what-is-control-w nil
      cua-lite-display-status-in-mode-line nil
      cua-lite-use-hscroll-mode nil)
(require 'cua-lite)
(cua-lite 1)
;; being able to minimize emacs isn't very helpful, especially if it
;; makes life harder for windows ppl
(if window-system (global-set-key (kbd "C-z") 'undo))

;; Overwrite highlighted text if you start typing
(delete-selection-mode t)
;; If we're typing on a region and press backspace, the region should die.
(global-set-key (kbd "C-d") 'delete-char-dwim)
(global-set-key (kbd "<DEL>") 'delete-backward-char-dwim)

;; These key-bindings make it very easy to navigate through grep
;; results, compile errors, etc.
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)

;; an oldy but a goody.  this makes the cursor stay in the same page
;; on the screen when you use `C-v` and `M-v` so you don't have to
;; keep re-centering with `C-l`
(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key (kbd "M-v") 'pager-page-up)

;; Function Keyz
(global-set-key [f3] 'lob/sudo-edit)
(global-set-key [f4] 'sr-speedbar-toggle) ;; We need to do something about those icons....
(global-set-key [f11] 'next-error)
(global-set-key [f12] 'previous-error)

;; Some additional file extensions
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; tiny scroll bars in minibuffer is silly
(set-window-scroll-bars (minibuffer-window) nil)

;; opposite of `M-q`
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; start emacs server, but not if some other emacs instance already
;; has a server
(if (fboundp 'server-start)
    (progn
      (require 'server) ;; server-running-p is not autoloaded
      (if (and (fboundp 'server-running-p) (not (server-running-p)))
          (server-start))))

;; Save backups in one place Put autosave files (ie #foo#) in one
;; place, *not* scattered all over the file system!
(defvar autosave-dir
  (expand-file-name "~/.lobstermacs/autosaves/"))
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

;; make emacs look/work better in the terminal
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; BIND DNS Zone Editing
(add-hook 'dns-mode-hook 'turn-off-flyspell)
;; Debian Style: /etc/bind/db.something.com
(add-to-list 'auto-mode-alist '("\\(bind\\|named\\)/db\\." . dns-mode))
;; RHEL Style: /var/lib/named/something.com.db
(add-to-list 'auto-mode-alist
	     '("\\.\\(net\\|com\\|org\\|info\\|us\\).db$" . dns-mode))

;; gettext translation files
(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . po-mode))
(modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\."
			    'po-find-file-coding-system)

;; DISABLED because it's irritating/astonishing to not be able to use
;; the terminal's copy/paste feature

;; lets you use the mouse in terminal mode.  seems to be a bug using
;; this on read-only buffers.  also highlighting doesn't take effect
;; until you let go of the mouse :\  ALSO this highlighting doesn't
;; seem to work when editing lisp files.
;;
;; (when (not window-system)
;;   (xterm-mouse-mode t))

;; DISABLED because it's buggy and seemed to make emacs flicker like
;; you wouldn't believe on a computer with a cheap monitor and
;; graphics card.

;; ;; Line numbers are cool but you can always have too much of a good
;; ;; thing.  Disable `linum-on` so it doesn't apply to popup buffers
;; ;; starting with '*' like `*Ido Completions*` and `*Help*`.  We're not
;; ;; going to bother with this on 4-bit terminals.
;; ;;
;; ;; BUG: flaky with company completion popups
;; (when (and (fboundp 'global-linum-mode)
;;            lob/is-colorful)
;;   (global-linum-mode 1)
;;   ;; in terminal there's no gutter, so give a little space
;;   (if (not window-system)
;;       (setq linum-format 'lob/linum-format-with-space)))
;; (defun linum-on ()
;;   (unless (or (minibufferp)
;;               (string-match "^\*" (buffer-name (current-buffer))))
;;     (linum-mode 1)))

(provide 'lobstermacs-misc)
;;; lobstermacs-misc.el ends here

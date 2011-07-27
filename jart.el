
;; my personal settings.  this is where i put all the configurations
;; i'm afraid others might not like.

(if (file-exists-p "/home/jart/.emacs-private")
    (load "/home/jart/.emacs-private"))

;; Make emacs a little nicer on dvorak keyboards
(global-set-key (kbd "C-u") ctl-x-map)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x C-h") 'help)

;; A few clashy shortcuts I feel are friendlier
(global-set-key (kbd "C-x C-t") 'other-window)
(global-set-key (kbd "C-x t") 'transpose-lines)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-v") vc-prefix-map)
(global-set-key (kbd "C-x C-g") 'grep-find)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; my function keys

(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f2>") 'jart/lobstermacs-build)
(global-set-key (kbd "<f4>") 'jart/face-at-point)
(global-set-key (kbd "C-<f4>") 'customize-apropos-faces)
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)

(global-set-key (kbd "<f6>") 'gud-next)
(global-set-key (kbd "C-<f6>") 'gud-nexti)
(global-set-key (kbd "<f7>") 'gud-step)
(global-set-key (kbd "C-<f7>") 'gud-stepi)
(global-set-key (kbd "<f8>") 'gud-finish)
(global-set-key (kbd "C-<f8>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-up)
(global-set-key (kbd "C-<f9>") 'gud-down)

(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "C-<f10>") 'gdb)
(global-set-key (kbd "C-u <f10>") 'jart/fix-gdb-gui)

(global-set-key (kbd "<f11>") 'next-error)
(global-set-key (kbd "<f12>") 'previous-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-extra-keywords (keywords mode &optional face)
  (let ((restr (c-regexp-opt
                (loop for s in keywords
                      collect (if (symbolp s) (format "%S" s) s))))
        (face (or face font-lock-keyword-face)))
    (font-lock-add-keywords mode `((,restr . ,face)))))

;; some mode specific stuff
(eval-after-load 'sh-script
  '(progn
     (define-key sh-mode-map (kbd "C-c v") 'lob/run-buffer)))
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)))
(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-u C-e") 'lob/python-eval)))
(eval-after-load 'cc-mode
  '(progn
     (add-extra-keywords '(ohai kthx) 'c-mode font-lock-preprocessor-face)
     (add-extra-keywords '(ohai kthx) 'c++-mode font-lock-preprocessor-face)
     (add-extra-keywords '(pure sentinel noreturn
                           nonnull deprecated hot cold) 'c++-mode)
     (add-extra-keywords '(pure sentinel noreturn __volatile__
						   __inline__ nonnull deprecated hot cold) 'c-mode)
     (define-key c-mode-map (kbd "C-h") c-backspace-function)))

;; various other configurations
;;(add-to-list 'auto-mode-alist '("\\.html$" . django-nxhtml-mumamo-mode))
(add-hook 'rst-mode-hook 'turn-off-flyspell)
(setq browse-url-browser-function 'browse-url-chrome)
(setq require-final-newline t)          ;; not having this drives me crazy
(setq ring-bell-function 'ignore)       ;; disable epilepsy
(setq visible-bell nil)                 ;; disable epilepsy
(setq lua-indent-level 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jart/fix-gdb-gui ()
  (interactive)
  (progn (gdb-many-windows t)
         (gdb-restore-windows)))

(defun lob/run-buffer ()
  (interactive)
  (shell-command buffer-file-name))

(defun jart/dedicate-window ()
  (interactive)
  (let ((win (get-buffer-window (current-buffer))))
    (set-window-dedicated-p win (not (window-dedicated-p win)))))

(defun jart/face-at-point ()
  "Tells me who is responsible for ugly color under cursor"
  (interactive)
  (message "%S: %s" (face-at-point)
           (face-documentation (face-at-point))))

(defun jart/lobstermacs-build ()
  "Recompiles everything so emacs loads wicked fast"
  (interactive)
  (regen-autoloads t)
  (recompile-init))

(defun jart/erc-on-connect (server nick)
  (when (string-match "freenode\\.net$" server)
    (if (equal nick "jart")
        (erc-message "PRIVMSG" (concat "NickServ identify " jart/freenode-password))))
  (when (string-match "xi01" server)
    (erc-join-channel "#halo")))
(eval-after-load 'erc
  '(add-hook 'erc-after-connect 'jart/erc-on-connect))


;; (require 'smex)
;; (eval-after-load "init.el" '(lambda ()
;;                               (smex-initialize)
;;                               (global-set-key (kbd "C-x C-m") 'smex)
;;                               (global-set-key (kbd "M-x") 'smex)
;;                               (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;                               (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;;                               ;; This is your old M-x.
;;                               (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

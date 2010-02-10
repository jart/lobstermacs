
;; my personal settings.  this is where i put all the configurations
;; i'm afraid others might not like.


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

;; my function keys
(global-set-key (kbd "<f2>") 'jart/lobstermacs-build)
(global-set-key (kbd "<f4>") 'jart/face-at-point)
(global-set-key (kbd "C-<f4>") 'customize-apropos-faces)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f9>") 'eshell)
(global-set-key (kbd "<f10>") 'compile)

(defun lob/run-buffer ()
  (interactive)
  (shell-command buffer-file-name))

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


;; various other configurations
(add-to-list 'auto-mode-alist '("\\.html$" . django-nxhtml-mumamo-mode))
(add-hook 'rst-mode-hook 'turn-off-flyspell)
(setq browse-url-browser-function 'browse-url-firefox)
(setq require-final-newline t)          ;; not having this drives me crazy
(setq ring-bell-function 'ignore)       ;; disable epilepsy
(setq visible-bell nil)                 ;; disable epilepsy


(defun jart/face-at-point ()
  "Tells me who is responsible for ugly color under cursor"
  (interactive)
  (message "%S: %s" (face-at-point)
           (face-documentation (face-at-point))))

(defun jart/lobstermacs-build ()
  "Recompiles everything so emacs loads wicked fast"
  (interactive)
  (regen-autoloads)
  (recompile-init))

;; (require 'smex)
;; (eval-after-load "init.el" '(lambda ()
;;                               (smex-initialize)
;;                               (global-set-key (kbd "C-x C-m") 'smex)
;;                               (global-set-key (kbd "M-x") 'smex)
;;                               (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;                               (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;;                               ;; This is your old M-x.
;;                               (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

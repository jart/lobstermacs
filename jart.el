
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
(global-set-key [f9] 'eshell)
(global-set-key [f10] 'compile)

;; some mode specific stuff
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


(defun jart/lobstermacs-build ()
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

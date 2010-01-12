
;; So my wrists can stay happy

(global-set-key (kbd "C-u") ctl-x-map)
(global-set-key (kbd "C-x C-v") vc-prefix-map)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-u C-e") 'lob/python-eval)))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-h") 'help)

(global-set-key (kbd "C-x C-t") 'other-window)
(global-set-key (kbd "C-x t") 'transpose-lines)

(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)

(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-g") 'grep-find)
(global-set-key (kbd "C-c C-f") 'goto-line)

(global-set-key [f9] 'eshell)
(global-set-key [f10] 'compile)

;; flyspell omg i don't need you complaining about every little
;; technical acronym :(
(add-hook 'rst-mode-hook 'turn-off-flyspell)

(add-to-list 'auto-mode-alist '("\\.html$" . django-nxhtml-mumamo-mode))

(setq browse-url-browser-function 'browse-url-firefox)
(setq require-final-newline t)
;; omg i don't want things to flicker and flash and beep every darn
;; time I hit `C-g`
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; (require 'smex)
;; (eval-after-load "init.el" '(lambda ()
;;                               (smex-initialize)
;;                               (global-set-key (kbd "C-x C-m") 'smex)
;;                               (global-set-key (kbd "M-x") 'smex)
;;                               (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;                               (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;;                               ;; This is your old M-x.
;;                               (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

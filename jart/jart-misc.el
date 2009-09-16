
;; flyspell omg i don't need you complaining about every little
;; technical acronym :(
(add-hook 'rst-mode-hook 'turn-off-flyspell)

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

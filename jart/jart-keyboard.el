
;; So my wrists can stay happy

(global-set-key (kbd "C-u") ctl-x-map)
(global-set-key (kbd "C-x C-v") vc-prefix-map)
(global-set-key (kbd "C-h") 'delete-backward-char-dwim)
(global-set-key (kbd "M-h") 'backward-kill-word)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)))

;; Some keybindings I like that others might not

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

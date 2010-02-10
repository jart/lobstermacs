;;; lobstermacs-c.el
;;
;; Part of Lobstermacs

;; change default style for curly brace langs from 'gnu' to 'linux'
(setq c-default-style
      (cons (cons 'other "linux")
            (assq-delete-all 'other c-default-style)))

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key c-mode-map (kbd "<return>") 'newline-and-indent)))

(provide 'lobstermacs-c)

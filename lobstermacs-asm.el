;;; lobstermacs-asm.el

(defun lob/asm-mode-hook ()
  (make-local-variable 'tab-width)
  (make-local-variable 'indent-tabs-mode)
  (setq tab-width 8)
  (setq indent-tabs-mode t))

(eval-after-load 'asm-mode
  '(progn
     (add-hook 'asm-mode-hook 'lob/asm-mode-hook)
     ))

(add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.ASM$" . asm-mode))

(provide 'lobstermacs-asm)

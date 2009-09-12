;;; starter-kit-python.el --- Better out of the box Python coding
;;
;; Part of the jart Emacs Starter Kit

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     (add-hook 'python-mode-hook 'run-coding-hook)
     (add-hook 'python-mode-hook 'idle-highlight)
     ))
     ;; (add-hook 'python-mode-hook 'esk-paredit-nonlisp)

;; (defun turn-on-company-python ()
;;   (require 'pymacs)
;;   ;;(setenv "PYMACS_PYTHON" (expand-file-name "~/code/myenv/bin/python"))
;;   ;;(pymacs-exec "import foo")
;;   (pymacs-load "ropemacs" "rope-")
;;   (require 'pysmell)
;;   (autoload 'company-mode "company" nil t)
;;   (company-mode))
;; (add-hook 'python-mode-hook 'turn-on-company-python)

(provide 'starter-kit-python)
;; starter-kit-python.el ends here

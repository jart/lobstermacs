
(require 'python)
(define-key python-mode-map (kbd "<return>") 'newline-and-indent)

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (require 'pymacs)
;;              ;;(setenv "PYMACS_PYTHON" (expand-file-name "~/code/myenv/bin/python"))
;;              ;;(pymacs-exec "import foo")
;;              (pymacs-load "ropemacs" "rope-")
;;              (require 'pysmell)
;;              (autoload 'company-mode "company" nil t)
;;              (company-mode)
;;              ))

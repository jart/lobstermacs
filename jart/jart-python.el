
(require 'python)
;;(require 'ipython)
(define-key python-mode-map (kbd "<return>") 'newline-and-indent)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/pysmell"))
(require 'pysmell)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/company"))
(autoload 'company-mode "company" nil t)

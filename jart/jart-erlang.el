
(require 'erlang)
(add-hook 'erlang-mode-hook
	  '(lambda ()
	     (set-fill-column 78)
	     (setq indent-tabs-mode nil)))

;; To get distel working, you have to have erlang installed and then
;; go into the distel folder and run make
;;
;; Ubuntu:
;; sudo apt-get install erlang erlang-dev erlang-doc

(add-to-list 'load-path (concat dotfiles-dir "/vendor/distel/elisp"))
(require 'distel)
(distel-setup)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))

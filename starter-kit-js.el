;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(add-to-list 'auto-mode-alist '("\\.js$\\|\\.json$" . js2-mode))

;; omg why won't it work?!?!?!

;; (autoload 'espresso-mode "espresso" "Start espresso-mode" t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
;; (add-hook 'espresso-mode-hook 'moz-minor-mode)
;; (add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
;; (add-hook 'espresso-mode-hook 'run-coding-hook)
;; (add-hook 'espresso-mode-hook 'idle-highlight)
;; (setq espresso-indent-level 2)

;; (eval-after-load 'espresso
;;   '(progn (load "paredit")
;;           (define-key espresso-mode-map "{" 'paredit-open-curly)
;;           (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
;;           ;; fixes problem with pretty function font-lock
;;           (define-key espresso-mode-map (kbd ",") 'self-insert-command)
;;           (font-lock-add-keywords
;;            'espresso-mode `(("\\(function *\\)("
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "ƒ")
;;                                        nil)))))))

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here

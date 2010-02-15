;;; lobstermacs-c.el
;;
;; In this file we'll try to make programming languages with curly
;; braces more fun to use
;;

(defun lob/on-c-closing-brace ()
  (interactive)
  (insert "}")
  (call-interactively 'c-indent-line-or-region))

(defun lob/on-c-mode-common-hook ()
  "Sets lobstermacs default settings for curly-braced languages"

  ;; don't make me press tab for every darn line
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

  ;; goto next line when i press `{`
  (c-toggle-auto-newline 1)
  (setq-default c-electric-pound-behavior '(alignleft))
  (define-key c-mode-base-map (kbd "}") 'lob/on-c-closing-brace)
  (define-key c-mode-base-map (kbd ";") 'self-insert-command)
  (setq c-hanging-semi&comma-criteria
	'(c-semi&comma-no-newlines-before-nonblanks
	  c-semi&comma-inside-parenlist))

  ;; happy keyboard things
  (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word))

(eval-after-load 'cc-vars
  '(progn
     (add-to-list 'c-mode-common-hook 'lob/on-c-mode-common-hook)

     ;; Set default styles
     ;;
     ;; "linux" styling is my personal favorite.  it makes a saner default
     ;; than "gnu" because it won't rain on your parade if you like to put
     ;; opening brackets on their own line.
     ;;
     ;; http://www.chris-lott.org/resources/cstyle/LinuxKernelCodingStyle.txt
     (setq-default c-default-style
                   (list (cons 'c-mode "linux")
                         (cons 'c++-mode "stroustrup")
                         (cons 'java-mode "java")
                         (cons 'awk-mode "awk")
                         (cons 'other "linux")))

     ;; add some more styles with intuitive names
     (loop for offset in (list 2 4 6 8)
           do (progn
                (c-add-style (format "indent-tabs-%d" offset)
                             `("linux"
                               (indent-tabs-mode . t)
                               (c-basic-offset . ,offset)))
                (c-add-style (format "indent-spaces-%d" offset)
                             `("linux"
                               (indent-tabs-mode . nil)
                               (c-basic-offset . ,offset)))))

     (c-add-style "microsoft"
                  '("stroustrup"
                    (c-offsets-alist
                     (innamespace . -)
                     (inline-open . 0)
                     (inher-cont . c-lineup-multi-inher)
                     (arglist-cont-nonempty . +)
                     (template-args-cont . +))))

     (c-add-style "openbsd"
                  '("bsd"
                    (indent-tabs-mode . t)
                    (defun-block-intro . 8)
                    (statement-block-intro . 8)
                    (statement-case-intro . 8)
                    (substatement-open . 4)
                    (substatement . 8)
                    (arglist-cont-nonempty . 4)
                    (inclass . 8)
                    (knr-argdecl-intro . 8)))))

(provide 'lobstermacs-c)

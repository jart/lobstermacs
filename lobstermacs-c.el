;;; lobstermacs-c.el
;;
;; In this file we'll try to make programming languages with curly
;; braces more fun to use
;;
;; Notes:
;;
;; If you want spaces in a c-like language, you can:
;;
;; a) Put this in your username.el or custom.el and restart emacs:
;;
;;      (setq lob/use-tabs-in-curly-langs t)
;;
;; b) Use one of the lobstermacs predefined styles:
;;
;;      (setq c-default-style "indent-tabs-2")
;;      (setq c-default-style "indent-tabs-4")
;;      (setq c-default-style "indent-tabs-5")
;;      (setq c-default-style "indent-tabs-8")
;;      (setq c-default-style "indent-spaces-2")
;;      (setq c-default-style "indent-spaces-4")
;;      (setq c-default-style "indent-spaces-5")
;;      (setq c-default-style "indent-spaces-8")
;;
;; c) Define your own custom style (based off linux, bsd, java, etc.)
;;
;;       (c-add-style "mystyle"
;; 	("linux"
;;          (indent-tabs-mode . nil)))
;;      (setq c-default-style "mystyle")
;;

(eval-when-compile (require 'cc-vars))

(defun lob/on-c-opening-brace ()
  "Extend electric brace mode to insert the closing brace.

We first run the normal `c-electric-brace' function which inserts
the opening curly brace and all sorts of other black magic.
After it's done we check if the current line changed (to be sure
we're not like in a comment or something) and then we insert the
closing brace.

If you've highlight some text, it'll take special care to make
sure the text you've highlighted goes inside the newly created
braces.

Hopefully you'll also have yank ident advice thing thing enabled
from lobstermacs-misc.el which will indent the pasted text."
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively 'kill-region)
        (if (string-match "^[ \t]*}" (lob/text-from-point-to-eol))
            (call-interactively 'open-line))
        (call-interactively '__lob/on-c-opening-brace)
        (call-interactively 'yank)
        (indent-according-to-mode))
    (call-interactively '__lob/on-c-opening-brace)))

(defun lob/text-from-point-to-eol (&optional point)
  (let ((pos (or point (point))))
    (buffer-substring-no-properties pos (save-excursion
                                          (goto-char pos)
                                          (line-end-position)))))

(defun __lob/on-c-opening-brace ()
  (interactive)
  (let ((curline (line-number-at-pos)))
    (call-interactively 'c-electric-brace)
    (unless (equal curline (line-number-at-pos))
      (save-excursion
        (call-interactively 'lob/on-c-mode-enter-key)
        (call-interactively 'lob/on-c-closing-brace))
      (call-interactively 'c-indent-line-or-region))))

(defun lob/on-c-closing-brace ()
  (interactive)
  (insert "}")
  (call-interactively 'c-indent-line-or-region))

(defun lob/on-c-mode-enter-key ()
  (interactive)
  ;; get current line under cursor
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    ;; does it look like a multi-line comment?
    (if (string-match "^[ \t]*/?\\*" line)
        ;; then we should insert the star when we press enter
        (c-indent-new-comment-line)
      ;; otherwise do a normal indent
      (newline-and-indent))))

(defun lob/on-c-mode-common-hook ()
  "Sets lobstermacs default settings for curly-braced languages"

  ;; http://www.delorie.com/gnu/docs/emacs/cc-mode_14.html
  ;; don't make me press tab for every darn line
  (define-key c-mode-base-map (kbd "RET") 'lob/on-c-mode-enter-key)
  (define-key c-mode-base-map (kbd "<return>") 'lob/on-c-mode-enter-key)
  (define-key c-mode-map (kbd "RET") 'lob/on-c-mode-enter-key)
  (define-key c-mode-map (kbd "<return>") 'lob/on-c-mode-enter-key)
  ;; (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  ;; Ctrl-Enter will properly continue a comment block
  (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)

  ;; def files are used to implement a C preprocessor design pattern
  ;; similar to C++ templates
  (add-to-list 'auto-mode-alist '("\\.def\\'" . c-mode))

  ;; goto next line when i press `{`
  (c-toggle-auto-newline 1)
  (setq-default c-electric-pound-behavior '(alignleft))
  (define-key c-mode-base-map (kbd "{") 'lob/on-c-opening-brace)
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

     ;; since we changed the emacs default to spaces only, we have to fix
     ;; some of the predefined styles to explicitly state they want tabs
     (loop for s in (list "linux" "k&r" "bsd" "stroustrup" "whitesmith"
                          "ellemtel" "java" "awk")
           do (c-add-style
               s (cons (cons 'indent-tabs-mode lob/use-tabs-in-curly-langs)
                       (c-get-style-variables s c-style-alist))))

     ;; ;; modify 'linux' style to enable more magic features like
     ;; ;; repositioning curly braces.  See `c-cleanup-list'.
     ;; ;;
     ;; ;; (insert (format "%S" (c-get-style-variables "linux" c-style-alist)))
     ;; ;;
     ;; ;; Why does lisp have to make everything so hard :'(
     ;; (c-add-style "linux"
     ;;              (let ((style (c-get-style-variables "linux" c-style-alist)))
     ;;                (cons (cons 'c-cleanup-list
     ;;                            (append '(brace-catch-brace
     ;;                                      brace-else-brace
     ;;                                      brace-elseif-brace)
     ;;                                    (cdr (assq 'c-cleanup-list style))))
     ;;                      style))

     (assoc 'c-cleanup-list (c-get-style-variables "linux" c-style-alist))

     ;; Set default styles
     ;;
     ;; "linux" styling is my personal favorite.  it makes a saner default
     ;; than "gnu" because it won't rain on your parade if you like to put
     ;; opening brackets on their own line.
     ;;
     ;; http://www.chris-lott.org/resources/cstyle/LinuxKernelCodingStyle.txt
     (setq-default c-default-style
                   (list (cons 'c-mode "linux")
                         (cons 'c++-mode "linux")
                         (cons 'java-mode "java")
                         (cons 'awk-mode "awk")
                         (cons 'other "linux")))

     ;; add some more styles with intuitive names
     (loop for offset in (list 2 4 5 8)
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

;;; lobstermacs-go.el
;;
;; Customizations for Google's Go Programming Language
;;

(defcustom go-outbuf-asm "*go-assembly*"
  "Output assembly to this buffer when I run \\[lob/go-show-asm]"
  :group 'go
  :type 'string)

(defun lob/go-mode-hook ()
  (make-local-variable 'tab-width)
  (setq tab-width 4))

(defun lob/go-show-asm ()
  "I show you what the Go code is your current buffer looks like
compiled to assembly.  This feature is pretty flakey so be warned!"
  (interactive)
  ;; save current position so we can jump to a different frame
  (let* ((sauce buffer-file-name)
         (curline (line-number-at-pos))
         (oldbuf (current-buffer))
         (olddbrf display-buffer-reuse-frames))
    ;; so emacs doesn't create a new frame every damn time
    (setq display-buffer-reuse-frames t)
    (unwind-protect
        (progn
          ;; if buffer is currently visible, go to that frame;
          ;; otherwise, create a new frame for asm output buffer
          (switch-to-buffer-other-frame go-outbuf-asm)
          (switch-to-buffer-other-frame oldbuf)
          ;; run compiler, dumping output to our buffer
          (if (equal 0 (shell-command (format "6g -S %s" sauce) go-outbuf-asm))
              ;; command succeeded, let's highlight asm for curline
              (progn
                (switch-to-buffer-other-frame go-outbuf-asm)
                ;; i know what file i'm editing thx
                (save-excursion (replace-string (concat sauce ":") "line "))
                ;; move cursor to anything referencing curline
                (while (search-forward (format "line %d)" curline) nil t)
                  (beginning-of-line)
                  ;; remove the line and then put it back with a
                  ;; different background face (ediff-current-diff-A)
                  (let ((line (buffer-substring-no-properties
                               (point) (+ 1 (line-end-position)))))
                    (delete-region (point) (+ 1 (line-end-position)))
                    (insert (propertize line 'face 'ediff-current-diff-A)))
                  ;; make sure it's vertically centered
                  (recenter))))
          ;; go back to where we were before
          (switch-to-buffer-other-frame oldbuf))
      ;; restore that setting we overwrote, even if we crashed
      (setq display-buffer-reuse-frames olddbrf))))

(autoload (quote go-mode) "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (cons "\\.go$" (function go-mode)))

(eval-after-load 'go-mode
  '(progn
     (define-key go-mode-map (kbd "RET") 'newline-and-indent)
     (define-key go-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key go-mode-map (kbd "C-<return>") 'c-indent-new-comment-line)
     (define-key go-mode-map (kbd "C-c C-a") 'lob/go-show-asm)
     (add-hook 'go-mode-hook 'lob/go-mode-hook)
     ))

(provide 'lobstermacs-go)

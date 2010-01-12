;;; lobstermacs-python.el --- Better out of the box Python coding
;;
;; Part of Lobstermacs.


(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-x C-e") 'lob/python-eval)
     (define-key python-mode-map [f10] 'lob/python-run)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     ;; (add-hook 'python-mode-hook 'idle-highlight)
     (add-hook 'python-mode-hook 'run-coding-hook)))


(defun lob/python-eval (&optional code)
  "Evaluate or Load Python Code into Emacs

Where the code comes from, ordered by priority:

  1. Explicitly passed to CODE argument
  2. Highlighted region
  3. Python statement or expression under cursor
  4. Ask user inside mini-buffer

This provides a simpler user interface to the `pymacs-eval' and
`pymacs-exec' functions.

Any variables you set or imports you make will persist between
requests (unless you call `pymacs-terminate-services'.)
"
  (interactive)
  (if (not (fboundp 'textwrap-dedent))
      (pymacs-load "textwrap" "textwrap-"))
  (let ((code (or code
                  (lob/highlighted-text)
                  (lob/current-python-statement)
                  (read-string "Python expression? "))))
    (let ((result (if (lob/is-python-expression code)
                      (pymacs-eval code)
                    (pymacs-exec (textwrap-dedent code)))))
      (if (interactive-p)
          (message "%S" result))
      result)))


(defun lob/python-run (&optional filepath)
  "Loads and Executes Python Script

This is essentially the same as highlighting your entire buffer
and running `lob/python-eval'.  The only differences are:

 * This will execute \"if __name__ == '__main__':\" code
 * It's less noisy in the `*Pymacs*' buffer (also faster)
"
  (interactive)
  (let ((path (or filepath
                  (if (or (string-match "\\.py$" (buffer-file-name))
                          (equal major-mode 'python-mode))
                      (progn
                        (if (buffer-modified-p)
                            (save-buffer))
                        (buffer-file-name)))
                  (ido-read-file-name "Python Script? "))))
    (pymacs-exec "__name__ = '__main__'")
    (unwind-protect
        (pymacs-exec (format "exec(open(%S).read())" path))
      (pymacs-exec "__name__ = 'Pymacs.pymacs'"))))


(defun lob/is-python-expression (code)
  "Determine if Python code can be sent to eval()

Returns true if CODE matches following criteria:

  * Doesn't have an assingment operator
  * Consists of a single statement or expression
  * Doesn't have any block statements like import, if, etc.
"
  (and
   (not (string-match "[ a-zA-Z0-9]=[ a-zA-Z\\]" code))
   (with-temp-buffer
     (insert code)
     (beginning-of-buffer)
     (python-end-of-statement)
     (null (search-forward-regexp "[\^ \t\r\n]" nil t)))
   (let ((keywords '(del while elif global with assert else if pass
                     yield import from print class raise try except
                     finally break continue return def for)))
     (let ((regex (mapconcat 'symbol-name keywords "\\|")))
       (null (string-match (concat "\n[ \t\r\n]*\\(" regex "\\)[\^a-za-z]")
                           (concat "\n" code "\n")))))))


(defun lob/current-python-statement ()
  "Returns the Python line of code under cursor

Smart enough to parse multi-line statements.  Your cursor can be
placed anywhere within the expression.
"
  (let ((start-statement (save-excursion (python-beginning-of-statement) (point)))
        (end-statement (save-excursion (python-end-of-statement) (point))))
    (if (and start-statement end-statement)
        (let ((current-line (buffer-substring-no-properties start-statement end-statement)))
          (if (and current-line
                   (string-match "[^ \t]" current-line)
                   (null (string-match ":[^ \t]*$" current-line)))
              current-line)))))


;; (defun turn-on-company-python ()
;;   (require 'pymacs)
;;   ;;(setenv "PYMACS_PYTHON" (expand-file-name "~/code/myenv/bin/python"))
;;   ;;(pymacs-exec "import foo")
;;   (pymacs-load "ropemacs" "rope-")
;;   (require 'pysmell)
;;   (autoload 'company-mode "company" nil t)
;;   (company-mode))
;; (add-hook 'python-mode-hook 'turn-on-company-python)


(provide 'lobstermacs-python)
;; lobstermacs-python.el ends here

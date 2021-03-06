;;; lobstermacs-python.el --- Better out of the box Python coding
;;
;; Part of Lobstermacs.

(defcustom lobstermacs-python-output-buffer "*python-output*"
  "When you evaluate Python code using `lob/python-eval',
anything your code prints will be sent to this buffer."
  :group 'lobstermacs-python
  :type 'string)


(defun lob/strip-pymacs-messages (data)
  "Removes Pymacs frames from a string.

This was written so we can extract the printed output of a Python
script from the Pymacs buffer without all the noise of Pymacs
chatting with the Python process.
"
  (let ((frame-start (string-match "^\\([><]\\([0-9]+\\)\t\\)" data)))
    (if frame-start
        (let ((frame-length (+ (length (match-string 1 data))
                               (string-to-number (match-string 2 data)))))
          (concat (substring data 0 frame-start)
                  (lob/strip-pymacs-messages
                   (substring data (+ frame-start frame-length) (length data)))))
      data)))


(defun lob/split-lines-no-blanks (data)
  "Split a string into a list of lines, removing blank lines"
  (loop for l in (split-string data "\n")
        if (string-match "[^ \t]" l)
        collect l))


(defun lob/python-eval (&optional code)
  "Evaluate or Load Python Code into Emacs

Where the code comes from, ordered by priority:

  1. Explicitly passed to CODE argument
  2. Highlighted region
  3. Python statement or expression under cursor
  4. Ask user inside mini-buffer

Program output will be shown in `lobstermacs-python-output-buffer'.

This provides a simpler user interface to the `pymacs-eval' and
`pymacs-exec' functions.

Any variables you set or imports you make will persist between
requests (unless you call `pymacs-terminate-services'.)
"
  (interactive)
  ;; make sure pymacs is running
  (unless (and pymacs-transit-buffer
               (buffer-name pymacs-transit-buffer)
               (get-buffer-process pymacs-transit-buffer))
    (pymacs-start-services))
  ;; get the code string itself
  (let ((code (or code
                  (lob/highlighted-text)
                  (lob/current-python-statement)
                  (read-string "Python expression? "))))
    ;; determine if we should use eval or exec
    (let* ((my-eval 'pymacs-eval)
           (my-exec '(lambda (code) (pymacs-exec (textwrap-dedent code))))
           (func-to-use (if (lob/is-python-expression code) my-eval my-exec)))
      ;; run the code
      (let ((before-output-pos (if (interactive-p)
                                   (save-excursion
                                     (set-buffer "*Pymacs*")
                                     (end-of-buffer)
                                     (beginning-of-line)
                                     (point))))
            (result (funcall func-to-use code)))
        ;; if a human ran me...
        (if (interactive-p)
            (let ((output (lob/split-lines-no-blanks
                           (lob/strip-pymacs-messages
                            (save-excursion
                              (set-buffer "*Pymacs*")
                              (buffer-substring-no-properties before-output-pos
                                                              (point-max)))))))
              ;; send printed output to a special buffer
              (if output
                  (save-excursion
                    (set-buffer (get-buffer-create lobstermacs-python-output-buffer))
                    (end-of-buffer)
                    (loop for line in output
                          do (insert (concat line "\n")))))
              ;; if we used eval() show result in minibuf
              (if (equal func-to-use my-eval)
                  (message "Result: %S" result)
                ;; if we used exec() show the last few lines
                (if output
                    (message
		     (concat (if (> (length output) 10)
				 (format "%d lines truncated (See %s buffer)\n[...]\n"
					 (- (length output) 10)
					 lobstermacs-python-output-buffer))
			     (mapconcat 'identity (last output 10) "\n")))))))
        result))))


(defun lob/python-eval-file (&optional filepath in_thread)
  "Loads and Executes Python Script

This is essentially the same as highlighting your entire buffer
and running `lob/python-eval'.  The only differences are:

 * This will execute \"if __name__ == '__main__':\" code
 * It's less noisy in the `*Pymacs*' buffer (also faster)
"
  (interactive)
  (let ((path (or filepath
                  (when (or (string-match "\\.py$" (buffer-file-name))
                            (equal major-mode 'python-mode))
                    (if (buffer-modified-p)
                        (save-buffer))
                    (buffer-file-name))
                  (ido-read-file-name "Python Script? "))))
    (if in_thread
        (pymacs-exec (format "
def lob_python_run():
    __name__ = '__main__'
    exec open(%S).read()
import threading
threading.Thread(target=lob_python_run).start()
" path))
      (pymacs-exec (format "
__name__ = '__main__'
try:     exec open(%S).read()
finally: __name__ = 'Pymacs.pymacs'
" path)))))


(defun lob/python-eval-file-in-thread (&optional filepath)
  "Same as `python-eval-file' except your code gets its own
namespace and runs in the background so it won't freeze up
emacs."
  (interactive)
  (lob/python-eval-file filepath t))


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
  (let ((start-statement (save-excursion
			   (python-beginning-of-statement) (point)))
        (end-statement (save-excursion
			 (python-end-of-statement) (point))))
    (if (and start-statement end-statement)
        (let ((current-line (buffer-substring-no-properties
			     start-statement end-statement)))
          (if (and current-line
                   (string-match "[^ \t]" current-line)
                   (null (string-match ":[^ \t]*$" current-line)))
              current-line)))))

(defun lob/python-check ()
  "Runs pyflakes and pep8 on current file

This will statically analyze your code to make sure you don't
have any obvious bugs and ensure that it conforms to the one true
coding style.

Use `next-error' and `previous-error' to hop between errors."
  (interactive)
  (let ((path (file-name-nondirectory buffer-file-name)))
    (compile (format "pyflakes %s ; pep8 --repeat %s" path path))))

(defun lob/python-check-dir ()
  "Same as `lob/python-check' but for all files in the current
directory (as well as sub-directories.)"
  (interactive)
  (compile "pyflakes . ; pep8 --repeat ."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)

     (define-key python-mode-map (kbd "C-c c") 'lob/python-check)
     (define-key python-mode-map (kbd "C-c C") 'lob/python-check-dir)

     (define-key python-mode-map (kbd "C-x C-e") 'lob/python-eval)
     (define-key python-mode-map (kbd "C-c v") 'lob/python-eval-file)
     (define-key python-mode-map [f10] 'lob/python-eval-file)
     (define-key python-mode-map (kbd "C-<f10>") 'lob/python-eval-file-in-thread)

     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     ;; (add-hook 'python-mode-hook 'idle-highlight)
     (add-hook 'python-mode-hook 'run-coding-hook)))

(eval-after-load 'pymacs
  '(progn
     (defadvice pymacs-start-services (after lob/on-pymacs-start)
       "Load our libraries whenever Pymacs starts a Python process"
       (pymacs-load "textwrap" "textwrap-")
       (assert (fboundp 'textwrap-dedent)))
     (ad-activate 'pymacs-start-services)))

;; ;; Uncomment this block to make this:
;; ;;    published = models.DateTimeField(auto_now_add=True, help_text="""
;; ;;                                     When blog post was published""")
;; ;; Become:
;; ;;    published = models.DateTimeField(auto_now_add=True, help_text="""
;; ;;        When blog post was published""")
;; ;; Credit: https://github.com/cwick/emacs
;; (defadvice python-calculate-indentation (around fix-list-indentation)
;;   "Fix indentation in continuation lines within lists"
;;   (unless
;;       (save-excursion
;;         (beginning-of-line)
;;         (when (python-continuation-line-p)
;;           (let* ((syntax (syntax-ppss))
;;                  (open-start (cadr syntax))
;;                  (point (point)))
;;             (when open-start
;;               ;; Inside bracketed expression.
;;               (goto-char (1+ open-start))
;;               ;; Indent relative to statement start, one
;;               ;; level per bracketing level.
;;               (goto-char (1+ open-start))
;;               (python-beginning-of-statement)
;;               (setq ad-return-value (+ (current-indentation)
;;                                        (* (car syntax) python-indent)))))))
;;     ad-do-it))

;; Makes this:
;; foo = {
;;     'bar': 'foobar',
;;     'foo': 42,
;;      }
;;
;; Become:
;; foo = {
;;     'bar': 'foobar',
;;     'foo': 42,
;; }
;; Credit: https://github.com/cwick/emacs
(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
  they line up with the line containing the corresponding opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (forward-char 1)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))

(ad-activate 'python-calculate-indentation)

(provide 'lobstermacs-python)
;; lobstermacs-python.el ends here

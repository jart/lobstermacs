;;; lobstermacs-python.el --- Better out of the box Python coding
;;
;; Part of Lobstermacs.


(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "<return>") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-x C-e") 'lob/pymacs-eval)
     (define-key python-mode-map (kbd "C-x C-e") 'lob/pymacs-eval)
     (define-key python-mode-map (kbd "M-:") 'pymacs-exec)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     (add-hook 'python-mode-hook 'run-coding-hook)
     (add-hook 'python-mode-hook 'idle-highlight)))


(defun lob/pymacs-eval (&optional code)
  "A far more intuitive alternative to pymacs-eval/pymacs-exec"
  (interactive)
  (if (not (fboundp 'textwrap-dedent))
      (pymacs-load "textwrap" "textwrap-"))
  (let ((code (or code
                  (lob/highlighted-text)
                  (lob/current-python-statement)
                  (read-string "Python expression? "))))
    (assert code)
    (let ((result (if (lob/is-python-statement code)
                      (pymacs-exec (textwrap-dedent code))
                    (pymacs-eval code))))
      (if (interactive-p)
          (message "%S" result)
        result))))

(defun lob/is-python-statement (code)
  (let* ((keywords '(del while elif global with assert else if pass
                     yield import from print class raise try except
                     finally break continue return def for))
         (keywords-regex (mapconcat 'symbol-name keywords "\\|")))
    (or (string-match (concat "^[ \t]*\\(" keywords-regex "\\)") code)
        (string-match (concat "\n[ \t]*\\(" keywords-regex "\\)") code))))

(defun lob/current-python-statement ()
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

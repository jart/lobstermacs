;;; lobstermacs-defuns.el --- Define some custom functions
;;
;; Part of Lobstermacs

(defun turn-on-company ()
  (company-mode t))

(defun lines (&rest lines)
  (mapconcat 'identity lines "\n"))
(defun padded-lines (&rest lines)
  (concat "\n\n" (mapconcat 'identity lines "\n") "\n\n"))

(defun lob/view-media-file (filename &optional force-browser)
  "Tries to figure out the best possible way to view a media file
like PDF, DVI, JPG, PNG, etc.  doc-view is the awesomest, then
we'll try evince, xpdf, and if all else fails we'll just throw it in the "
  (interactive "fFile? ")
  ;; Example:
  ;;   (lob/view-media-file "/home/jart/Documents/Markdown_Cheat_Sheet.pdf")
  (cond
   ((and (not force-browser)
         (fboundp 'image-mode)
         (memq (intern (downcase (file-name-extension filename)))
               '(jpg jpeg png gif tif tiff svg svgz)))
    (find-file filename))
   ((and (not force-browser)
         (fboundp 'doc-view-mode)
         (memq (intern (downcase (file-name-extension filename)))
               '(pdf dvi epdf epdf ps eps)))
    (find-file filename))
   ((and (not force-browser)
         (memq (intern (downcase (file-name-extension filename)))
               '(pdf dvi))
         (executable-find "evince"))
    (shell-command (concat "evince " (expand-file-name filename))))
   ((and (not force-browser)
         (memq (intern (downcase (file-name-extension filename)))
               '(pdf dvi))
         (executable-find "xpdf"))
    (shell-command (concat "xpdf " (expand-file-name filename))))
   (t (browse-url (concat "file://" (expand-file-name filename))))))

(defun lob/regex-match (regexp string &optional number)
  "Easier regular expression matching"
  ;; Example:
  ;;   (lob/regex-match "\\(hel\\)\\(lo\\)" "hello" 2)
  ;;       => "lo"
  ;;   (lob/regex-match "ID:\t\\(\\w+\\)\n" "Distributor ID:	Ubuntu\n" 1)
  ;;       => "Ubuntu"
  (if (string-match regexp string)
      (match-string (or number 0) string)))

(defmacro lob/nevar-fail (primary failover)
  "Runs primary code.  If primary code fails, then executes
  failover code."
  `(condition-case exc
       ,primary
     ('error
      (message (format "Caught exception: [%s]" exc))
      ,failover)))

;; (defmacro lob/safen-command (command on-fail)
;;   "Declare a defun named 'command-safe' that will perform
;;   'on-fail' if something bad happens."
;;   `(defun ,(intern (concat (symbol-name command) "-safe")) ()
;;      (lob/nevar-fail (,command) ,on-fail)))

;; (defmacro lob/safen-command (command on-fail)
;;   "Declare a defun named 'command-safe' that will perform
;;   'on-fail' if something bad happens."
;;   `(defun ,(make-symbol (concat (symbol-name command) "-safe")) ()
;;      (interactive)
;;      (condition-case exc
;;          (apply ,command)
;;        ('error
;;         (message (format "Caught exception: [%s]" exc)))
;;        ,on-fail)))

;; (call-interactively 'paredit-close-parenthesis)
;; (lob/safen-command paredit-close-parenthesis (insert ")"))
;; (lob/safen-command paredit-close-parenthesis-and-newline (insert ")"))
;; (macroexpand-all '(lob/safen-command paredit-close-parenthesis (insert ")")))
;; (paredit-close-parenthesis-safe)

(defun paredit-close-parenthesis-safe ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis)
                  (insert ")")))

(defun paredit-close-parenthesis-and-newline-safe ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis-and-newline)
                  (insert ")")))

;; credit: chris capel's emacs file
(defun close-or-bury-window ()
  "Closes the open window or buries it if it's the only
open window.  Example:

    (progn
      (erlang-shell)
      (close-or-bury-window))
"
  (interactive)
  (if (= (count-windows) 1)
      (bury-buffer)
          (delete-window)))

(defun delete-char-dwim ()
  (interactive)
  (if mark-active
      (delete-active-region)
    (delete-char 1)))

(defun delete-backward-char-dwim ()
  (interactive)
  (if mark-active
      (delete-active-region)
    (delete-backward-char 1)))

(defun sudo-edit-me ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

(defun lob/linum-format-with-space (line)
  "This is here because in terminal mode the fringe goes away,
the line numbers touch your code, it's really ugly.  You would
normally just say:

  (setq linum-format \"%d \")

But the numbers won't right align so do this:

  (setq linum-format 'lob/linum-format-with-space)
"
  (propertize
   (format
    (let ((w (length (number-to-string
                      (count-lines (point-min)
                                   (point-max))))))
      (concat "%" (number-to-string w) "d ")) line) 'face 'linum))

;; IDO M-x is cool :D
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(provide 'lobstermacs-defuns)
;;; lobstermacs-defuns.el ends here

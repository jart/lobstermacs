;;; lobstermacs-defuns.el --- Define some custom functions
;;
;; Part of Lobstermacs

(defun lob/warn (msg)
  "ToDo: Have LobsterMacs warnings go in special buffer to avoid
unhelpful noise in *Messages*"
  (message (concat "\n*** WARNING: " msg "\n")))

(defun lob/strip (s)
  (if (string-match "^[ \t]*\\([^ \t]*\\)[ \t]*$" s)
      (match-string 1 s)
    s))

(defun lob/highlighted-text ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))))

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

(defun lob/sudo-edit (&optional path)
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

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

(defun unfill-paragraph ()
  "The opposite of fill-paragraph. Takes a multi-line paragraph
and makes it into a single line of text.  Thanks: Stefan Monnier
<foo at acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun lob/is-text-mode ()
  "Is current buffer text-mode, or derived from text-mode?  I was
going to use this a way to selectively enable
`lob/try-complete-ispell` but adding it to the end of the
hippie-expand list seems to work wonders."
  (or (string= "text-mode" (symbol-name major-mode))
      (string= "text-mode" (symbol-name (derived-mode-class major-mode)))))

(defun lob/is-editing-english ()
  "Returns true if \\[lob/is-text-mode] or \\[fundamental-mode],
otherwise detects if cursor is inside a comment by font face at
current cursor point to see if it matches
\\[font-lock-comment-face] or \\[font-lock-doc-face]"
  (or (lob/is-text-mode)
      (string= "fundamental-mode" (symbol-name major-mode))
      (string= "font-lock-comment-face" (symbol-name (get-char-property (point) 'face)))
      (string= "font-lock-doc-face" (symbol-name (get-char-property (point) 'face)))))

(autoload 'lookup-words "ispell" "Lookup word for dictionary completions." nil)

(defun lob/try-complete-ispell (old)
  "Making hippie-expand a little better for text-mode and stuff.
I based this code off `try-complete-lisp-symbol` and
`ispell-complete-word`"
  (when (lob/is-editing-english)
    (if (not old)
          (progn
            (he-init-string (he-lisp-symbol-beg) (point))
            (if (not (he-string-member he-search-string he-tried-table))
                (setq he-tried-table (cons he-search-string he-tried-table)))
            (setq he-expand-list
                  (and (not (equal he-search-string ""))
                       (lookup-words he-search-string ispell-complete-word-dict)))))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (if old (he-reset-string))
            ())
        (progn
          (he-substitute-string (car he-expand-list))
          (setq he-expand-list (cdr he-expand-list))
          t))))

(provide 'lobstermacs-defuns)
;;; lobstermacs-defuns.el ends here

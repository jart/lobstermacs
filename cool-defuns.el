
(defun strip-trailing-whitespace ()
  "remove all whitespace from the end of lines in the entire buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+$" nil t)
      (replace-match ""))))

(defun load-my-favorite-file ()
  "Open my favorite file: ~.emacs.
The function is poorly named, didn't really want to 'load' it, just open it."
  (interactive)
  (find-file "~/.emacs"))

(defvar cycle-special-files-file-list
  '("~/.mailrc" "~/personal/.phones""~/public_html/home.html"))

(defun cycle-special-files ()
  "cycle through the set of files specified in cycle-special-files-file-list."
  (interactive)
  (let* ((cur-buffer (current-buffer))
         (buff-list (mapcar (lambda (f) (find-file-noselect (file-truename f)))  cycle-special-files-file-list))
         (2x-buff-list (append buff-list buff-list))
         (default (car buff-list))
         (res (memq cur-buffer 2x-buff-list)))

    ;; find buffer after the current one
    (if (and res (cdr res))
        (setq default (cadr res)))

    ;; bury the buffer if it's one of the special list
    (when (member cur-buffer buff-list) (bury-buffer cur-buffer))
    (when default
      (switch-to-buffer default))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-string-matches (strn)
  "Return number of matches STRING following the point.
Continues until end of buffer.  Also display the count as a message."
  (interactive (list (read-string "Enter string: ")))
  (save-excursion
    (let ((count -1))
      (while 
      (progn
        (setq count (1+ count))
        (search-forward strn nil t)))
      (message "%d matches" count)
      count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar insert-code-template-templates
  '(((c-mode c++-mode) ".h$" "/* copyright 2001 */\n#ifndef __SOMETHING_H\n#define __SOMETHING_H\n\n#endif /* # __SOMETHING_H */\n")
    ((c-mode c++-mode) nil "int\nmain(int argc, char **argv)\n{\n\n}\n")
    ((cperl-mode perl-mode) nil "#!/usr/bin/perl -w\n\nuse strict;\n"))
  "A list of triples, used for inserting code.
A triplet is composed of a symbol for the major mode (or a list of symbols),
a regular expression to match against the buffer's file name,
and the text to insert when both the major mode and regular expression match.")

(defun insert-code-template ()
  "insert a code template, based on major mode when called
interactively, always do insertion otherwise, only do so when the
buffer is empty"
  (interactive)
  (let ((l insert-code-template-templates))
    (when (or (called-interactively-p)
              (eq (point-min) (point-max)))
      (while l
        (let* ((elt (car l))
               (modes (if (listp (car elt)) (car elt) (list (car elt))))
               (re (cadr elt))
               (template (caddr elt)))
          (when (and (member major-mode modes)
                     (or (null re)
                         (string-match re (buffer-file-name))))
            (insert template)
            (setq l nil)))
        (setq l (cdr l))))))

(add-hook 'find-file-hook 'insert-code-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-insert-checkin-form (branch type date priority description module files dependencies other)
  "Insert a checkin form string into current buffer.
Called interactively, will prompt for fields."
  (interactive (list (read-string "Branch (default \"TOT\"): " nil nil "TOT")
                     (read-string "Check-in Type: (default \"internal enhancement\"): " nil nil "internal enhancement")
                     (format-time-string "%B %e, %Y")
                     (read-string "Priority: " "med")
                     (read-string "Description: ")
                     (read-string "Module: ")
                     (read-string "Files: ")
                     ""
                     ""))
  (let* ((deps-other-str (if (not (and (= 0 (length dependencies))
                                       (= 0 (length other))))
                             (format "
Dependencies:   %s
Other:          %s
" dependencies other)
                           ""))
         (checkin-string-format (format "Branch:         %%s
Check-in Type:  %%s
When Ready:     %%s
Priority:       %%s%s
Description:    %%s
Module:         %%s
Files:          %%s
" deps-other-str))
         (checkin-cvs-format "cvs ci -m '%s' %s"))

    ;; first insert the check-in notice
    (insert (format checkin-string-format branch type date priority description module (my-format-files-for-checkin-string files)))
    ;; put a few newlines in
    (insert "\n\n\n\n")
    ;; then insert a cvs commit line
    (insert (format checkin-cvs-format (concat type " " description) (my-format-files-for-checkin-cvs files)))))

;; relies on string-replace http://snipplr.com/view/18683/stringreplace/
(defun my-format-files-for-checkin-string (fstr)
  "replace all whitespace in string with a newline and 16 spaces"
  (string-replace "\\s-+" "
                " fstr t))

(defun my-format-files-for-checkin-cvs (fstr)
  "replace all whitespace in string with a space"
  (string-replace "\\s-+" " " fstr t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; straight-forward solution doing just string manipulation
;; it's rather heavy handed
(defun string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.
When optional fourth argument is non-nil, treat the from as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))

;; the more emacs way to do it is to use 
;; string-replace - which has the drawback of not knowing *where*
;; the replacement was done, so if you apply this technique to strings
;; you can end up in an infinite loop
;; try:   (string-replace "a" "aa" "a string contains a")
;; the "trick" is to put the string into a temporary buffer, like so:

(defun string-replace-2 (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

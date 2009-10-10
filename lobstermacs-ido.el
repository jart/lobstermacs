;;; lobstermacs-ido.el --- Make the best feature even better
;;
;; Part of Lobstermacs

;; Use IDO for extended execute (M-x)
;;
;; ToDo: After done starting up, add a hook to all eval functions or
;;"advise" to get this dumb cache regenerated
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

;; This seems to make other stuff use ido like `C-h f`, `C-h v`, etc.
;; Stolen from 'InteractivelyDoThings' on EmacsWiki
;;
;; *** Disabled because it seems to break some stuff up like opening a
;; TAGS file with `M-.` ***
;;
;; (defadvice completing-read
;;   (around foo activate)
;;   (if (boundp 'ido-cur-list)
;;       ad-do-it
;;     (setq ad-return-value
;;           (ido-completing-read
;;            prompt
;;            (all-completions "" collection predicate)
;;            nil require-match initial-input hist def))))

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(provide 'lobstermacs-ido)
;;; lobstermacs-ido.el ends here

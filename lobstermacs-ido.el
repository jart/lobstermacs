;;; lobstermacs-ido.el --- Make the best feature even better
;;
;; Part of Lobstermacs

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

;; ;; Switch buffers with IDO using `C-Tab` and `C-S-Tab`
;; ;; this doesn't seem to work right now :'(
;; (defun lob/ido-next-match ()
;;   (interactive)
;;   (if (active-minibuffer-window)
;;       (ido-next-match)
;;     (ido-switch-buffer)))
;; (global-set-key [(control tab)]       'lob/ido-next-match)
;; (global-set-key [(control shift tab)] 'ido-prev-match)
;; (global-set-key (kbd "C-S-<iso-lefttab>") 'ido-prev-match)
;; ;(define-key ido-buffer-completion-map (kbd "C-S-<iso-lefttab>") 'ido-prev-match)
;; ;(define-key ido-buffer-completion-map [(control shift tab)] 'ido-prev-match)

;; ;; *** Disabled because it seems to break some stuff up like opening a
;; ;; TAGS file with `M-.` ***
;; ;;
;; ;; This seems to make other stuff use ido like `C-h f`, `C-h v`, etc.
;; ;; Stolen from 'InteractivelyDoThings' on EmacsWiki
;; (defadvice completing-read
;;   (around foo activate)
;;   (if (boundp 'ido-cur-list)
;;       ad-do-it
;;     (setq ad-return-value
;;           (ido-completing-read
;;            prompt
;;            (all-completions "" collection predicate)
;;            nil require-match initial-input hist def))))

(provide 'lobstermacs-ido)
;;; lobstermacs-ido.el ends here

;;; lobstermacs-post-custom.el --- Special configurations
;;
;; Part of Lobstermacs
;;
;; These configurations are loaded after the user's `custom.el` file
;; has loaded but before their `<username>.el` file loads.  This is
;; important because the custom file has a tendency to completely
;; overwrite some lists when we'd like to add stuff to them.

;; switching to *SPEEDBAR* buffer is pointless when you can press F4
;; to toggle its display.  let's make ido ignore it
(add-to-list 'ido-ignore-buffers "\\*SPEEDBAR\\*")

;; TODO: Fix me, I am buggy :'(
;; ;; add ispell support to hippie-expand
;; (if lob/have-ispell
;;     (add-to-list 'hippie-expand-try-functions-list 'lob/try-complete-ispell))

(provide 'lobstermacs-post-custom)
;;; lobstermacs-defuns.el ends here

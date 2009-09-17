;;; lobstermacs-rst.el --- Restructured Text (ReST) Support
;;
;; Part of Lobstermacs.

(defcustom rst-rst2pdf-command "rst2pdf"
  "Command used to generate rst2pdf previews")

(eval-after-load 'rst
  '(progn
     (add-hook 'rst-adjust-hook 'rst-toc-update)
     (define-key rst-mode-map (kbd "C-c 4") 'rst-preview-pdf)
     (define-key rst-mode-map (kbd "C-c 5") 'rst-preview-pdf-latex)
     (define-key rst-mode-map (kbd "C-c 6") 'rst-preview-html)))

(defun rst-preview-html ()
  (interactive)
  (let ((out-file (concat temporary-file-directory "rst2html_output.html")))
    (if (= 0 (shell-command (format "rst2html %s %s" buffer-file-name out-file)))
        (lob/view-media-file out-file)
      (error (lines "Sorry I didn't work ._."
                    "Try this:\n"
                    "  sudo apt-get install python-docutils")))))

(defun rst-preview-pdf ()
  "Takes the current restructured text buffer, generates a PDF
with the very excellent rst2pdf program which typesets with
ReportLab instead of TeX, and then displays that PDF file either
inside emacs or in your web browser.

TODO: Look for a sheet file like /usr/share/doc/rst2pdf/examples/montecristo/montecristo.sheet"
  (interactive)
  (let ((out-file-pdf (concat temporary-file-directory "rst2pdf_output.pdf")))
    (if (= 0 (shell-command (format "rst2pdf -s fruity.json -o '%s' '%s'"
                                    out-file-pdf buffer-file-name) "*compilation*"))
        (progn
          ;; (delete-window (get-buffer-window (get-buffer "*TeX Output*")))
          ;; (file-exists-p out-file-pdf)
          (lob/view-media-file out-file-pdf))
      (progn
        (if (get-buffer "*Shell Command Output*")
            (switch-to-buffer (get-buffer "*Shell Command Output*")))
        ;; (if (get-buffer "*TeX Output*")
        ;;     (switch-to-buffer (get-buffer "*TeX Output*")))
        (error (lines "Sorry I didn't work ._."
                      "Try this:\n"
                      "  sudo easy_install rst2pdf"
                      "-or-"
                      "  sudo apt-get install rst2pdf"))))))

(defun rst-preview-pdf-latex ()
  "Takes the current restructured text buffer, generates a
beautiful PDF file with LaTeX typesetting, and then displays that
PDF file either inside emacs or in your web browser."
  (interactive)
  (let ((out-file-tex (concat temporary-file-directory "rst2latex_output.tex"))
        (out-file-pdf (concat temporary-file-directory "rst2latex_output.pdf")))
    (if (and (= 0 (shell-command (format "rst2newlatex '%s' '%s'"
                                         buffer-file-name out-file-tex)))
             (if (executable-find "pdflatex")
                 (= 0 (shell-command (format "pdflatex -output-directory '%s' '%s'"
                                             temporary-file-directory out-file-tex)
                                     "*TeX Output*"))
               (= 0 (shell-command (format "texi2pdf --build-dir %s -o '%s' '%s'"
                                           temporary-file-directory out-file-pdf out-file-tex)
                                   "*TeX Output*"))))
        (progn
          (delete-window (get-buffer-window (get-buffer "*TeX Output*")))
          (file-exists-p out-file-pdf)
          (lob/view-media-file out-file-pdf))
      (progn
        (if (get-buffer "*TeX Output*")
            (switch-to-buffer (get-buffer "*TeX Output*")))
        (error (lines "Sorry I didn't work ._."
                      "Try this:\n"
                      "  sudo apt-get install python-docutils texlive-latex-extra \\"
                      "      texlive-lang-french texlive-latex-base"))))))

(provide 'lobstermacs-rst)
;;; lobstermacs-rst.el ends here

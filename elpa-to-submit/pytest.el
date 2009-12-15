;;; pytest -- Run unit tests which output to collapsable sect menus

;; Copyright (C) 2009  J.A. Roberts Tunney
;;
;; Based on Magit developed by:
;; Copyright (C) 2008, 2009  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Alex Ott
;; Copyright (C) 2008  Marcin Bachry
;; Copyright (C) 2009  Alexey Voinov
;; Copyright (C) 2009  John Wiegley
;;
;; Emacs PyTest is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Emacs PyTest is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs PyTest.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary

;;; Developer Commentary

;; `M-x sect-describe` is very helpful when viewing the output.  When
;; using edebug, pressing `e` to evaluate section variables isn't very
;; helpful so press `e (sect-describe varname)` instead!

;;; TODO

;; * Blah

(require 'cl)
(require 'parse-time)
(require 'log-edit)
(require 'easymenu)
(require 'diff-mode)
(require 'sect)

;;; Customizations

(defgroup pytest nil
  "Runs py.test unit tests from Emacs."
  :prefix "pytest-"
  :group 'tools)

(defcustom pytest-executable "py.test"
  "The name of the py.test executable.  Gets overriden if py.test in a virtual environment"
  :group 'pytest
  :type 'string)

(defcustom pytest-standard-options "-xl"
  "Command line options to pass"
  :group 'pytest
  :type 'string)

;;; Fonts

(defface pytest-failure-title
  '((t :inherit diff-file-header))
  "Face for section titles."
  :group 'pytest)

(defface pytest-frame-title
  '((t :inherit diff-hunk-header))
  "Face for each function thingy in backtrace"
  :group 'pytest)

(defface pytest-code-text
  '((t :inherit default))
  "Face for chunks of code."
  :group 'pytest)

(defface pytest-evil-text
  '((t :background "DarkRed" :foreground "White" :bold))
  "Face for evil error text"
  :group 'pytest)

(defface pytest-happy-text
  '((t :background "DarkGreen" :foreground "White" :bold))
  "Face for text when things don't crash and burn"
  :group 'pytest)

(defface pytest-variable-name
  '((t :inherit bold))
  "Face for name of variable in backtrace stack frames"
  :group 'pytest)

(defface pytest-variable-value
  '((t :inherit default))
  "Face for the contents of variables in stack frame"
  :group 'pytest)

;;; Structures

(defstruct pytest-session
  cmd cmd-opts cmd-dir platform
  num-failed num-passed seconds
  failures)

(defstruct pytest-failure
  test-name captured-stdout captured-stderr
  frames)

(defstruct pytest-frame
  error-type ;; pytest only sets this for top frame
  before-lines line after-lines error-eval-lines
  relfile absfile lineno
  func-args vars)

(defstruct pytest-var
  name value)

;;; Mode Code

(defvar pytest-mode-hook nil)

(defvar pytest-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'sect-goto-next-section)
    (define-key map (kbd "p") 'sect-goto-previous-section)
    (define-key map (kbd "TAB") 'sect-toggle-section)
    (define-key map (kbd "q") 'quit-window)
    map))

(defun pytest-mode ()
  "Allows you to get down like James Brown.

\\{pytest-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq major-mode 'pytest-mode
	mode-name "PyTest"
	mode-line-process ""
	truncate-lines t
	line-move-visual nil)
  (add-hook 'pre-command-hook #'pytest-remember-point nil t)
  (add-hook 'post-command-hook #'pytest-post-command-hook t t)
  (use-local-map pytest-mode-map)
  (run-mode-hooks 'pytest-mode-hook))

(defun pytest-mode-init (dir refresh-func &rest refresh-args)
  (setq default-directory dir
	pytest-refresh-function refresh-func
	pytest-refresh-args refresh-args)
  (pytest-mode)
  (pytest-refresh-buffer))

(defun pytest ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*lol*"))
  (let ((topdir "/home/jart/code/nemesis/src/nemesis/nemesis"))
    (setq buffer-read-only nil)
    (widen)
    (delete-region (point-min) (point-max))
    (pytest-mode-init topdir #'pytest-refresh-status)))

(put 'pytest-mode 'mode-class 'special)

;;; Point Correction

(defvar last-point)

(defun pytest-remember-point ()
  (setq last-point (point)))

(defun pytest-invisible-region-end (pos)
  (while (and (not (= pos (point-max))) (invisible-p pos))
    (setq pos (next-char-property-change pos)))
  pos)

(defun pytest-invisible-region-start (pos)
  (while (and (not (= pos (point-min))) (invisible-p pos))
    (setq pos (1- (previous-char-property-change pos))))
  pos)

(defun pytest-correct-point-after-command ()
  ;; Emacs often leaves point in invisible regions, it seems.  To fix
  ;; this, we move point ourselves and never let Emacs do its own
  ;; adjustements.
  ;;
  ;; When point has to be moved out of an invisible region, it can be
  ;; moved to its end or its beginning.  We usually move it to its
  ;; end, except when that would move point back to where it was
  ;; before the last command.
  ;;
  (if (invisible-p (point))
      (let ((end (pytest-invisible-region-end (point))))
	(goto-char (if (= end last-point)
		       (pytest-invisible-region-start (point))
		     end))))
  (setq disable-point-adjustment t))

;;; Rendering / Washing

(defun pytest-post-command-hook ()
  (pytest-correct-point-after-command)
  (sect-highlight-section))

(defun pytest-refresh-status ()
  (setq pytest-executable "/home/jart/code/nemesis/bin/py.test")
  (sect-create-buffer-sections
    (sect-with-section 'status nil
      (pytest-insert-pytest)))
  (goto-char (point-min))
  (sect-goto-next-section)
  (sect-expand-section)
  (sect-goto-next-section))

(defun pytest-insert-pytest ()
  (sect-insert-section 'pytest
                       nil
                       'pytest-wash-pytest
                       pytest-executable pytest-standard-options))

(defun pytest-wash-pytest ()
  (pytest-display-session (pytest-parse-output)))

(defun pytest-display-session (session)
  "Given a \\[pytest-section], outputs all those cool collapsable
menus to the buffer."
  ;; summary
  (insert (format "Command:   %s %s\n"
                  (pytest-session-cmd session)
                  (pytest-session-cmd-opts session))
          (format "Directory: %s\n" (pytest-session-cmd-dir session))
          (format "Platform:  %s\n" (pytest-session-platform session))
          (format "Result:    %s\n" (pytest-format-summary session))
          "\n")
  ;; failures (backtraces)
  (mapl '(lambda (fail)
           (pytest-display-failure (car fail)))
        (pytest-session-failures session)))

(defun pytest-display-failure (failure)
  (let* ((test-name (pytest-failure-test-name failure))
         (top-frame (car (pytest-failure-frames failure)))
         (error-type (pytest-frame-error-type top-frame))
         (error-line (pytest-frame-line top-frame))
         (error-eval-lines (pytest-frame-error-eval-lines top-frame))
         (section-title (format "! %s @ %s" error-type test-name))
         (sect-section-hidden-default nil))
    (sect-with-section test-name 'failure
      (sect-set-section-info test-name)
      (insert (propertize section-title 'face 'pytest-failure-title) "\n")
      (insert "======================================================================\n")
      (insert "\n")
      (insert (propertize error-line 'face 'pytest-code-text))
      (insert (propertize error-eval-lines 'face 'pytest-evil-text))
      (insert "\n")
      (mapl '(lambda (frame)
               (pytest-display-frame (car frame)))
            (pytest-failure-frames failure)))))

(defun pytest-display-frame (frame)
  (let* ((absfile (pytest-frame-absfile frame))
         (relfile (pytest-frame-relfile frame))
         (lineno (pytest-frame-lineno frame))
         (sect-section-hidden-default nil))
    (sect-with-section '(absfile lineno) 'frame
      (sect-set-section-info '(absfile lineno))
      (insert (propertize (format "+ %s:%d" relfile lineno) 'face 'pytest-frame-title) "\n")
      (insert "----------------------------------------------------------------------\n")
      (insert "\n")
      (insert (propertize (pytest-frame-before-lines frame) 'face 'pytest-code-text))
      (insert (propertize (pytest-frame-line frame) 'face 'pytest-code-text))
      (insert (propertize (pytest-frame-error-eval-lines frame) 'face 'pytest-evil-code-text))
      (insert (propertize (pytest-frame-after-lines frame) 'face 'pytest-code-text))
      (insert "\n")
      (let ((args (pytest-frame-func-args frame))
            (vars (pytest-frame-vars frame))
            (dispvar #'(lambda (var)
                         (insert (format "%s = %s\n"
                                         (pytest-var-name (car var))
                                         (pytest-var-value (car var)))))))
        (if args
            (sect-with-section "omfg" 'vars
              (sect-set-section-info '(absfile lineno))
              (insert (propertize "+ FUNCTION ARGUMENTS" 'face 'pytest-frame-title) "\n")
              (mapl dispvar args)
              (insert "\n")))
        (if vars
            (sect-with-section "omfg" 'vars
              (sect-set-section-info '(absfile lineno))
              (insert (propertize "* VARIABLES" 'face 'pytest-frame-title) "\n")
              (mapl dispvar vars)
              (insert "\n")))))))

(defun pytest-parse-output ()
  "Return \\[pytest-session] struct or fails.  Looks in current
buffer for py.test output and assumes \\[default-directory] is
the folder in which py.test was run."
  (let ((session (make-pytest-session :cmd pytest-executable
                                      :cmd-opts pytest-standard-options
                                      :cmd-dir default-directory
                                      :platform (pytest-get-platform)
                                      :failures (list))))
    (pytest-extract-summary session)
    (pytest-kill-keyboard-interrupt)
    (setf (pytest-session-failures session)
          (let (failures failure)
            (while (set 'failure (pytest-get-failure))
              (set 'failures (cons failure failures)))
            failures))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    session))

(defun pytest-get-failure ()
  "Returns \\[pytest-failure] struct if frame is found, otherwise nil."
  (let ((start-failure-regexp "^___+ \\([^ ]+\\) ___"))
    (while (and (not (eobp))
                (not (looking-at start-failure-regexp)))
      (delete-region (point) (+ (line-end-position) 1)))
    (if (looking-at start-failure-regexp)
        (let ((failure (make-pytest-failure :test-name (match-string-no-properties 1))))
          (delete-region (point) (+ (line-end-position) 1))
          (setf (pytest-failure-frames failure) (pytest-get-frames))
          failure))))

(defun pytest-get-frames ()
  "Narrow to entire backtrace for this failure, then for each
frame, narrow down to its contents and extract the data."
  (let (framez suicide)
    (save-excursion
      (save-restriction
        (narrow-to-region (point) (pytest-find-end-of-backtrace))
        (goto-char (point-min))
        (while (set 'suicide (save-excursion
                               (save-restriction
                                 (narrow-to-region (point) (pytest-find-end-of-frame))
                                 (goto-char (point-min))
                                 (pytest-get-frame))))
          (set 'framez (cons suicide framez)))))
    framez))

(defun pytest-get-frame ()
  "Returns \\[pytest-frame] struct if frame is found, otherwise nil.

Then comes the function variables (including function arguments
because they might have changed) which are represented in the
about the same manner as the function args."
  (goto-char (point-max))
  (if (search-backward-regexp "^\\([^:]+\\):\\([0-9]+\\): \\([^\n]*\\)" nil t)
      (let ((frame (make-pytest-frame
                    :relfile (match-string-no-properties 1)
                    :absfile (expand-file-name
                              (concat default-directory "/"
                                      (match-string-no-properties 1)))
                    :lineno (string-to-number (match-string-no-properties 2))
                    :error-type (match-string-no-properties 3)
                    :before-lines ""
                    :after-lines ""
                    :error-eval-lines "")))
        (goto-char (point-min))
        ;; function arguments
        (setf (pytest-frame-func-args frame)
              (pytest-parse-vars))
        ;; function code where it failed
        (pytest-extract-context frame)
        ;; local variables
        (setf (pytest-frame-vars frame)
              (pytest-parse-vars))
        ;; all done!
        (delete-region (point-min) (point-max))
        frame)
    nil))

(defun pytest-extract-context (frame)
  "Extracts error line and surronding line information from
  py.test output buffer and sets it to frame structure.

The function code is always indented with four spaces (even if
it's module-scope,) or begins with a '> ' which indicates the
last line to be executed.

If this is the top-most (or last to be printed) frame, you may
also get some lines starting with 'E' which is py.test magic
which expands the variables values on the error line, which is a
pretty neat feature."
  (pytest-skip-blank-lines)
  (while (and (not (eobp))
              (not (looking-at "^[ \t]$")))
    (cond ((looking-at "^    [^\n]+$")
           (setf (pytest-frame-before-lines frame)
                 (concat (pytest-frame-before-lines frame)
                         (match-string-no-properties 0) "\n")))
          ((looking-at "^E   [^\n]+$")
           (setf (pytest-frame-error-eval-lines frame)
                 (concat (pytest-frame-error-eval-lines frame)
                         (match-string-no-properties 0) "\n")))
          ((looking-at "^>   [^\n]+$")
           (setf (pytest-frame-line frame)
                 (concat (match-string-no-properties 0) "\n"))))
    (forward-line)))

(defun pytest-parse-vars ()
  "If the first non blank line looks like a variable, this code
kicks in until it finds another blank line.  Returns list of
\\[pytest-var] structs.  Variables in same cases may not be
present so nil will be returned."
  (pytest-skip-blank-lines)
  (if (looking-at "^[_[:alnum:]]+ +=")
      (let ((vars (list)))
        (while (and (not (eobp))
                    (not (looking-at "^[ \t]$")))
          (cond
           ;; new variable
           ((looking-at "^\\([_[:alnum:]]+\\) += +\\([^\n]+\\)$")
            (set 'vars (cons (make-pytest-var :name (match-string-no-properties 1)
                                              :value (match-string-no-properties 2)) vars)))
           ;; multi-line variable continuation
           ((looking-at "^[^\n]+$")
            (setf (pytest-var-value (car vars))
                  (concat (pytest-var-value (car vars)) "\n"
                          (match-string-no-properties 0))))
           (t
            (error "omg what is this line??")))
          (forward-line))
        vars)))

;;; Helper Functions

(defun pytest-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if pytest-refresh-function
        (apply pytest-refresh-function
               pytest-refresh-args))))

(defun pytest-extract-summary (session)
  (save-excursion
    (goto-char (point-max)) ;; this info always on last line
    (search-backward-regexp "\\([0-9]+\\) failed, \\([0-9]+\\) passed in \\([.0-9]+\\) seconds")
    (setf (pytest-session-num-failed session)
          (string-to-number (match-string-no-properties 1)))
    (setf (pytest-session-num-passed session)
          (string-to-number (match-string-no-properties 2)))
    (setf (pytest-session-seconds session)
          (string-to-number (match-string-no-properties 3)))))

(defun pytest-format-summary (session)
  (let ((failed (number-to-string (pytest-session-failed session)))
        (passed (number-to-string (pytest-session-passed session)))
        (seconds (number-to-string (pytest-session-seconds session))))
    (concat
     (propertize (concat failed " failed") 'face 'pytest-evil-text) ", "
     (propertize (concat passed " passed") 'face 'pytest-happy-text) " in "
     (propertize (concat seconds " seconds") 'face 'bold))))

(defun pytest-get-platform ()
  (save-excursion
    (search-forward-regexp "^python: \\(.+\\)$")
    (match-string-no-properties 1)))

(defun pytest-kill-keyboard-interrupt ()
  "If they hit Ctrl-C, remove that \"!!! KEYBOARD INTERRUPT !!!\"
message and the line comes after it."
  (save-excursion
    (goto-char (point-max))
    (if (search-backward-regexp "!!! KEYBOARD INTERRUPT !!!" nil t)
        (progn
          (beginning-of-line)
          (delete-region (point) (+ (line-end-position) 1))
          (if (looking-at "^[^:]+:[0-9]+: ")
              (delete-region (point) (+ (line-end-position) 1)))))))

(defun pytest-wash-sequence (func)
  "Keeps calling function until we hit the end of the buffer"
  (while (and (not (eobp))
	      (funcall func))))

(defun pytest-goto-line (line)
  "Like goto-line but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

(defun pytest-find-end-of-frame ()
  (save-excursion
    (condition-case ex
        (progn
          (search-forward-regexp "^_ _ _ _ _")
          (beginning-of-line)
          (delete-region (point) (+ (line-end-position) 1))
          (point))
      ('error (point-max)))))

(defun pytest-find-end-of-backtrace ()
  (save-excursion
    (condition-case ex
        ;; look for end of py.test session
        (progn
          (search-forward-regexp "^=======")
          (beginning-of-line)
          (point))
      ;; look for start of a new failure backtrace
      ('error (progn (search-forward-regexp "^_______")
                     (beginning-of-line)
                     (point))))))

(defun pytest-skip-blank-lines ()
  (while (and (not (eobp))
              (looking-at "^[ \t]$"))
    (forward-line)))

(provide 'pytest)

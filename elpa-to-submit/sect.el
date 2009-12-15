;;; sect -- Hierarchical Collapsable Sections

;; Adapted from Magit source by jart
;;
;; Copyright (C) 2009  J.A. Roberts Tunney
;; Copyright (C) 2008, 2009  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Alex Ott
;; Copyright (C) 2008  Marcin Bachry
;; Copyright (C) 2009  Alexey Voinov
;; Copyright (C) 2009  John Wiegley
;;
;; sect is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; sect is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sect.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; sect allows you to make certain types of buffers prettier and
;; easier to use with collapsable sections.  This allows you hide
;; superfluous information like VCS diffs, compilation output, local
;; variables in backtraces, etc.

;; When I originally discovered Magit, an Emacs frontend for git, I
;; instantly fell in love with how it worked and was inspired by its
;; way of doing things.  I created this library because I felt that
;; the way Magit organizing data into collapsable sections could
;; greatly improve many other parts of Emacs.

;;; Customization Stuff

(require 'cl)

(defgroup sect nil
  "Hierarchical collapsable sections"
  :prefix "sect-"
  :group 'tools)

(defface sect-header
  '((t))
  "Face for generic header lines.

Many Pytest faces inherit from this one by default."
  :group 'sect)

(defface sect-section-title
  '((t :weight bold :inherit sect-header))
  "Face for section titles."
  :group 'sect)

(defface sect-item-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for highlighting the current item."
  :group 'sect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Incomprehensible Code Goes Here

(defstruct sect-section
  parent title beginning end children hidden type info)

(defvar sect-top-section nil)
(make-variable-buffer-local 'sect-top-section)
(put 'sect-top-section 'permanent-local t)

(defvar sect-old-top-section nil)

(defvar sect-section-hidden-default nil)

(defun sect-new-section (title type)
  (let* ((s (make-sect-section :parent sect-top-section
				:title title
				:type type
				:hidden sect-section-hidden-default))
	 (old (and sect-old-top-section
		   (sect-find-section (sect-section-path s)
				       sect-old-top-section))))
    (if sect-top-section
	(setf (sect-section-children sect-top-section)
	      (cons s (sect-section-children sect-top-section)))
      (setq sect-top-section s))
    (if old
	(setf (sect-section-hidden s) (sect-section-hidden old)))
    s))

(defun sect-cancel-section (section)
  (delete-region (sect-section-beginning section)
		 (sect-section-end section))
  (let ((parent (sect-section-parent section)))
    (if parent
	(setf (sect-section-children parent)
	      (delq section (sect-section-children parent)))
      (setq sect-top-section nil))))

(defmacro sect-with-section (title type &rest body)
  (declare (indent 2))
  (let ((s (gensym)))
    `(let* ((,s (sect-new-section ,title ,type))
	    (sect-top-section ,s))
       (setf (sect-section-beginning ,s) (point))
       ,@body
       (setf (sect-section-end ,s) (point))
       (setf (sect-section-children ,s)
	     (nreverse (sect-section-children ,s)))
       ,s)))

(defun sect-set-section-info (info &optional section)
  (setf (sect-section-info (or section sect-top-section)) info))

(defmacro sect-create-buffer-sections (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((sect-old-top-section sect-top-section))
       (setq sect-top-section nil)
       ,@body
       (when (null sect-top-section)
	 (sect-with-section 'top nil
	   (insert "(empty)\n")))
       (sect-propertize-section sect-top-section)
       (sect-section-set-hidden sect-top-section
				 (sect-section-hidden sect-top-section)))))

(defun sect-propertize-section (section)
  (put-text-property (sect-section-beginning section)
		     (sect-section-end section)
		     'sect-section section)
  (dolist (s (sect-section-children section))
    (sect-propertize-section s)))

(defun sect-find-section (path top)
  (if (null path)
      top
    (let ((sec (find-if (lambda (s) (equal (car path)
					   (sect-section-title s)))
			(sect-section-children top))))
      (if sec
	  (sect-find-section (cdr path) sec)
	nil))))

(defun sect-section-path (section)
  (if (not (sect-section-parent section))
      '()
    (append (sect-section-path (sect-section-parent section))
	    (list (sect-section-title section)))))

(defun sect-find-section-at (pos secs)
  (while (and secs
	      (not (and (<= (sect-section-beginning (car secs)) pos)
			(<  pos (sect-section-end (car secs))))))
    (setq secs (cdr secs)))
  (if secs
      (or (sect-find-section-at pos (sect-section-children (car secs)))
	  (car secs))
    nil))

(defun sect-find-section-after (pos secs)
  (while (and secs
	      (not (> (sect-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun sect-find-section-before (pos secs)
  (let ((prev nil))
    (while (and secs
		(not (> (sect-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun sect-current-line-str ()
  (buffer-substring-no-properties (line-beginning-position)
				  (line-end-position)))
(defun sect-current-section ()
  (or (get-text-property (point) 'sect-section)
      sect-top-section))

(defun sect-insert-section (section-title-and-type
			     buffer-title washer cmd &rest args)
  (let* ((body-beg nil)
	 (section-title (if (consp section-title-and-type)
			    (car section-title-and-type)
			  section-title-and-type))
	 (section-type (if (consp section-title-and-type)
			   (cdr section-title-and-type)
			 nil))
	 (section
	  (sect-with-section section-title section-type
	    (if buffer-title
		(insert (propertize buffer-title 'face 'sect-section-title)
			"\n"))
	    (setq body-beg (point))
	    (apply 'process-file cmd nil t nil args)
	    (if (not (eq (char-before) ?\n))
		(insert "\n"))
	    (if washer
		(save-restriction
		  (narrow-to-region body-beg (point))
		  (goto-char (point-min))
		  (funcall washer)
		  (goto-char (point-max)))))))
    (if (= body-beg (point))
	(sect-cancel-section section)
      (insert "\n"))
    section))

(defun sect-next-section (section)
  (let ((parent (sect-section-parent section)))
    (if parent
	(let ((next (cadr (memq section
				(sect-section-children parent)))))
	  (or next
	      (sect-next-section parent))))))

(defun sect-goto-next-section ()
  (interactive)
  (let* ((section (sect-current-section))
	 (next (or (and (not (sect-section-hidden section))
			(sect-section-children section)
			(sect-find-section-after (point)
						  (sect-section-children
						   section)))
		   (sect-next-section section))))

    (if next
	(progn
	  (goto-char (sect-section-beginning next))
	  (if (not (sect-section-hidden next))
	      (let ((offset (- (line-number-at-pos
				(sect-section-beginning next))
			       (line-number-at-pos
				(sect-section-end next)))))
		(if (< offset (window-height))
		    (recenter offset)))))
      (message "No next section"))))

(defun sect-prev-section (section)
  (let ((parent (sect-section-parent section)))
    (if parent
	(let ((prev (cadr (memq section
				(reverse (sect-section-children parent))))))
	  (cond (prev
		 (while (and (not (sect-section-hidden prev))
			     (sect-section-children prev))
		   (setq prev (car (reverse (sect-section-children prev)))))
		 prev)
		(t
		 parent))))))

(defun sect-goto-previous-section ()
  (interactive)
  (let ((section (sect-current-section)))
    (let ((prev (sect-find-section-before (point)
                                          (sect-section-children
                                           section))))
      (goto-char (sect-section-beginning (or prev section))))))

(defun sect-goto-section (path)
  (let ((sec (sect-find-section path sect-top-section)))
    (if sec
	(goto-char (sect-section-beginning sec))
      (message "No such section"))))

(defun sect-for-all-sections (func &optional top)
  (let ((section (or top sect-top-section)))
    (when section
      (funcall func section)
      (dolist (c (sect-section-children section))
	(sect-for-all-sections func c)))))

(defun sect-section-set-hidden (section hidden)
  (setf (sect-section-hidden section) hidden)
  (let ((inhibit-read-only t)
	(beg (save-excursion
	       (goto-char (sect-section-beginning section))
	       (forward-line)
	       (point)))
	(end (sect-section-end section)))
    (put-text-property beg end 'invisible hidden))
  (if (not hidden)
      (dolist (c (sect-section-children section))
	(sect-section-set-hidden c (sect-section-hidden c)))))

(defun sect-section-any-hidden (section)
  (or (sect-section-hidden section)
      (some #'sect-section-any-hidden (sect-section-children section))))

(defun sect-section-collapse (section)
  (dolist (c (sect-section-children section))
    (setf (sect-section-hidden c) t))
  (sect-section-set-hidden section nil))

(defun sect-section-expand (section)
  (dolist (c (sect-section-children section))
    (setf (sect-section-hidden c) nil))
  (sect-section-set-hidden section nil))

(defun sect-section-expand-all-aux (section)
  (dolist (c (sect-section-children section))
    (setf (sect-section-hidden c) nil)
    (sect-section-expand-all-aux c)))

(defun sect-section-expand-all (section)
  (sect-section-expand-all-aux section)
  (sect-section-set-hidden section nil))

(defun sect-section-hideshow (flag-or-func)
  (let ((section (sect-current-section)))
    (cond ((sect-section-parent section)
	   (goto-char (sect-section-beginning section))
	   (if (functionp flag-or-func)
	       (funcall flag-or-func section)
	     (sect-section-set-hidden section flag-or-func))))))

(defun sect-show-section ()
  (interactive)
  (sect-section-hideshow nil))

(defun sect-hide-section ()
  (interactive)
  (sect-section-hideshow t))

(defun sect-collapse-section ()
  (interactive)
  (sect-section-hideshow #'sect-section-collapse))

(defun sect-expand-section ()
  (interactive)
  (sect-section-hideshow #'sect-section-expand))

(defun sect-toggle-section ()
  (interactive)
  (sect-section-hideshow
   (lambda (s)
     (sect-section-set-hidden s (not (sect-section-hidden s))))))

(defun sect-expand-collapse-section ()
  (interactive)
  (sect-section-hideshow
   (lambda (s)
     (cond ((sect-section-any-hidden s)
	    (sect-section-expand-all s))
	   (t
	    (sect-section-collapse s))))))

(defun sect-cycle-section ()
  (interactive)
  (sect-section-hideshow
   (lambda (s)
     (cond ((sect-section-hidden s)
	    (sect-section-collapse s))
	   ((notany #'sect-section-hidden (sect-section-children s))
	    (sect-section-set-hidden s t))
	   (t
	    (sect-section-expand s))))))

(defun sect-section-lineage (s)
  (and s (cons s (sect-section-lineage (sect-section-parent s)))))

(defun sect-section-show-level (section level threshold path)
  (sect-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
	(sect-section-show-level (car path) (1+ level) threshold (cdr path))
      (dolist (c (sect-section-children section))
	(sect-section-show-level c (1+ level) threshold nil)))))

(defun sect-show-level (level all)
  (if all
      (sect-section-show-level sect-top-section 0 level nil)
    (let ((path (reverse (sect-section-lineage (sect-current-section)))))
      (sect-section-show-level (car path) 0 level (cdr path)))))

(defmacro sect-define-level-shower-1 (level all)
  (let ((fun (intern (format "sect-show-level-%s%s"
			     level (if all "-all" ""))))
	(doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (sect-show-level ,level ,all))))

(defmacro sect-define-level-shower (level)
  `(progn
     (sect-define-level-shower-1 ,level nil)
     (sect-define-level-shower-1 ,level t)))

(defmacro sect-define-section-jumper (sym title)
  (let ((fun (intern (format "sect-jump-to-%s" sym)))
	(doc (format "Jump to section `%s'." title)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (sect-goto-section '(,sym)))))

(defvar sect-highlight-overlay nil)

(defvar sect-highlighted-section nil)

(defun sect-highlight-section ()
  (let ((section (sect-current-section)))
    (when (not (eq section sect-highlighted-section))
      (setq sect-highlighted-section section)
      (if (not sect-highlight-overlay)
	  (let ((ov (make-overlay 1 1)))
	    (overlay-put ov 'face 'sect-item-highlight)
	    (setq sect-highlight-overlay ov)))
      (if (and section (sect-section-type section))
	  (move-overlay sect-highlight-overlay
			(sect-section-beginning section)
			(sect-section-end section)
			(current-buffer))
	(delete-overlay sect-highlight-overlay)))))

(defun sect-section-context-type (section)
  (if (null section)
      '()
    (let ((c (or (sect-section-type section)
		 (if (symbolp (sect-section-title section))
		     (sect-section-title section)))))
      (if c
	  (cons c (sect-section-context-type
		   (sect-section-parent section)))
	'()))))

(defun sect-prefix-p (prefix list)
  ;;; Very schemish...
  (or (null prefix)
      (if (eq (car prefix) '*)
	  (or (sect-prefix-p (cdr prefix) list)
	      (and (not (null list))
		   (sect-prefix-p prefix (cdr list))))
	(and (not (null list))
	     (equal (car prefix) (car list))
	     (sect-prefix-p (cdr prefix) (cdr list))))))

(defmacro sect-section-case (head &rest clauses)
  (declare (indent 1))
  (let ((section (car head))
	(info (cadr head))
	(type (gensym))
	(context (gensym))
	(opname (caddr head)))
    `(let* ((,section (sect-current-section))
	    (,info (sect-section-info ,section))
	    (,type (sect-section-type ,section))
	    (,context (sect-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
			 (if (eq (car clause) t)
			     clause
			   (let ((prefix (reverse (car clause)))
				 (body (cdr clause)))
			     `((sect-prefix-p ',prefix ,context)
			       ,@body))))
		       clauses)
	     ,@(if opname
		   `(((not ,type)
		      (error "Nothing to %s here." ,opname))
		     (t
		      (error "Can't %s a %s."
			     ,opname
			     (or (get ,type 'sect-description)
				 ,type)))))))))

(defmacro sect-section-action (head &rest clauses)
  (declare (indent 1))
  `(sect-with-refresh
     (sect-section-case ,head ,@clauses)))

(defun sect-wash-sequence (func)
  (while (and (not (eobp))
	      (funcall func))))

(defun sect-describe (&optional section)
  (interactive)
  (let ((section (or section (sect-current-section))))
    (message "Section: %s %s-%s %S %S %S"
	     (sect-section-type section)
	     (sect-section-beginning section)
	     (sect-section-end section)
	     (sect-section-title section)
	     (sect-section-info section)
	     (sect-section-context-type section))))

(provide 'sect)

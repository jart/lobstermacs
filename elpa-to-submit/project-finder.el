;;; project-finder -- Heuristics for guessing where your project at

;; Copyright (C) 2009 Lobstertech, Inc.

;; Author: J.A. Roberts Tunney <jtunney@lobstertech.com>
;; Version: 0.1
;; Keywords: tools
;; Created: 2009-10-11
;; URL: http://www.lobstertech.com/

;; This file is NOT part of GNU Emacs.

;; project-finder is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Blah

;;; TODO:

;; * Stuff

;;; Code:

(defun lob/python-root-package-folder (&optional dir)
  "Returns name of parent dir whose parent has no
  \"__init__.py\" file"
  (let ((dir (expand-file-name (or dir default-directory))))
    (if (file-exists-p (concat dir "/__init__.py"))
        (if (file-exists-p (concat dir "/../__init__.py"))
            (lob/python-root-package-folder (concat dir "/.."))
          (expand-file-name dir))
      (expand-file-name dir))))

(defun lob/python-project-folder (&optional dir)
  "Starts off with \\[lob/python-root-package-folder] but returns
parent directory if the parent dir contains a `setup.py` file."
  (let ((pdir (lob/python-root-package-folder dir)))
    (if (file-exists-p (concat pdir "/../setup.py"))
        (expand-file-name (concat pdir "/.."))
      pdir)))

(provide 'project-finder)
;;; project-finder.el ends here

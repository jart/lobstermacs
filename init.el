;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; mess with gui early on to avoid a dancing flickering emacs window

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(unless window-system
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(when window-system
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (add-hook 'before-make-frame-hook 'turn-off-tool-bar))
(setq frame-title-format '(buffer-file-name "%f  -  lobstermacs" ("%b")))

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/company"))
;; (add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/nxhtml"))
;; (add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/nxhtml/util"))
;; (if (>= emacs-major-version 23)
;;     (load (expand-file-name (concat dotfiles-dir "/elpa-to-submit/nxhtml/autostart.el")))
;;   (load (expand-file-name (concat dotfiles-dir "/elpa-to-submit/nxhtml/autostart22.el"))))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Python load path
(setenv "PYTHONPATH" (expand-file-name (concat dotfiles-dir "/python/lib/")))

;; until they fix the elpa package thing
(autoload 'company-mode "company" nil t)

;; Load these earlier, they contain important system checks and gui
;; changes

(require 'lobstermacs-defuns)
(require 'lobstermacs-system)
;; enable zenburn theme if we have 256+ colors
(when (and lob/is-colorful (not (featurep 'zenburn)))
  (require 'zenburn)
  (color-theme-zenburn))
(load custom-file 'noerror)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'mercurial)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up ELPA, the package manager

(require 'package)

;; another weird buggy (something about yas/minor-mode-menu)
(condition-case ex
    (package-initialize)
  ('error (load "yasnippet-bundle.el")
	  (package-initialize)))

(require 'starter-kit-elpa)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

;; lobstermacs stuff
;; (require 'lobstermacs-ido)
(require 'lobstermacs-c)
(require 'lobstermacs-rst)
(require 'lobstermacs-lisp)
(require 'lobstermacs-python)
(require 'lobstermacs-erlang)
(require 'lobstermacs-erc)
(require 'lobstermacs-misc)

(regen-autoloads)

(yas/load-directory (concat dotfiles-dir "/snippets"))

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;; init.el ends here

;;; lobstermacs-system.el --- Auto-Configures emacs for your OS
;;
;; Part of Lobstermacs

;; detect different operating systems
(setq lob/is-windows (not (null (memq system-type '(ms-dos windows-nt cygwin)))))
(setq lob/is-unix (not (null (memq system-type '(gnu/linux darwin berkeley-unix cygwin)))))
(setq lob/is-linux (not (null (memq system-type '(gnu/linux)))))
(setq lob/is-mac (not (null (memq system-type '(darwin)))))

;; detect different linux distros
(setq lob/lsb-release (if lob/is-linux
                          (shell-command-to-string "lsb_release -irc")))
(setq lob/linux-distro (if lob/is-linux (downcase (lob/regex-match "ID:\t\\(.+\\)$" lob/lsb-release 1))))
(setq lob/linux-distro-release (if lob/is-linux (downcase (lob/regex-match "Release:\t\\(.+\\)$" lob/lsb-release 1))))
(setq lob/linux-distro-codename (if lob/is-linux (downcase (lob/regex-match "Codename:\t\\(.+\\)$" lob/lsb-release 1))))

;; ubuntu
(setq lob/is-ubuntu (and lob/is-linux (string= lob/linux-distro "ubuntu")))
(setq lob/is-ubuntu-karmic (and lob/is-ubuntu (string= lob/linux-distro-codename "karmic")))
(setq lob/is-ubuntu-jaunty (and lob/is-ubuntu (string= lob/linux-distro-codename "jaunty")))
(setq lob/is-ubuntu-intrepid (and lob/is-ubuntu (string= lob/linux-distro-codename "intrepid")))
(setq lob/is-ubuntu-hardy (and lob/is-ubuntu (string= lob/linux-distro-codename "hardy")))

;; means terminal or emacs gui supports at least 256 colors
;; in gnome terminal i can run:
;;
;;   TERM=xterm-256color emacs
;;
(setq lob/is-colorful (>= (display-color-cells) 256))

(if (and (not window-system)
         (not lob/is-colorful))
    (lob/warn (concat "If you want pretty terminal colors use: "
                      "'export TERM=xterm-256color' before running me!")))

;; spelling is fun
(if (fboundp 'ispell)
    (require 'ispell))
(setq lob/have-cmd-ispell (not (null (executable-find "ispell"))))
(setq lob/have-dictionary-file (and (fboundp 'ispell) (file-exists-p ispell-alternate-dictionary)))
(setq lob/have-ispell (and (fboundp 'ispell)
                           (or lob/have-cmd-ispell lob/have-dictionary-file)))
(if (not lob/have-ispell)
    ;; they NOTHING, no ispell, no dictionary file for grepping :(
    (lob/warn (concat "ispell could not be found on your system!  "
                      "Spell checking and completion will not work :( "
                      "Please run: sudo apt-get install ispell"))
  ;; they have a dictionary file, but not the efficient ispell program
  (if (not lob/have-cmd-ispell)
      (lob/warn (concat "For more efficient spell checking and completion, "
                        "please install ispell: sudo apt-get install ispell"))))

(provide 'lobstermacs-system)
;;; lobstermacs-misc.el ends here

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
(setq lob/linux-distro (downcase (lob/regex-match "ID:\t\\(.+\\)$" lob/lsb-release 1)))
(setq lob/linux-distro-release (downcase (lob/regex-match "Release:\t\\(.+\\)$" lob/lsb-release 1)))
(setq lob/linux-distro-codename (downcase (lob/regex-match "Codename:\t\\(.+\\)$" lob/lsb-release 1)))

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

(provide 'lobstermacs-system)
;;; lobstermacs-misc.el ends here

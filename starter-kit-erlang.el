;;; starter-kit-erlang.el --- Erlang/Distel Support
;;
;; Part of the jart's Emacs Starter Kit.

;; SETUP

;; To get distel working, you have to have erlang installed and then
;; go into the distel folder and run make
;;
;; Ubuntu:
;; sudo apt-get install erlang erlang-dev erlang-doc erlang-manpages

;; NOTE: You should create a `~/.erlang` file listing where all your
;; Erlang sauce is located.  This way when you press `C-c C-z` to
;; start an Erlang shell, Erlang can find all those goodies.  Here is
;; an example:
;;
;; code:add_pathsz(["/home/jart/code/emacs-starter-kit/vendor/distel/ebin",
;;                  "/home/jart/code/switchboard/deps/mochiweb/ebin",
;;                  "/home/jart/code/switchboard/deps/ibrowse/ebin"]).
;;
;; You might also want to create a `~/.erlang.cookie` file with a
;; bunch of random letters on one line.  I'm not sure if that helps.
;;
;; If Erlang is in a weird place this might help
;;
;; (setq erlang-root-dir "/usr/local/otp")
;; (setq exec-path (cons "/usr/local/otp/bin" exec-path))

;; YOU CAN TELL ERLANG TO DO THINGS
;;
;; (erl-eval-expression "mynode@server" "suffer().")
;; (erl-eval-expression (concat "emacs@" system-name) "code:add_pathsz([\"/var/code/ebin\"]).")
;; (erl-eval-expression (erl-target-node) "2 + 2.")

;; make sure erlang knows about 2 + 2 being 4
;; (erl-spawn
;;   (erl-send-rpc "emacs@compy"        ; node name
;;                 'distel              ; module name
;;                 'eval_expression     ; function name
;;                 (list "2 + 2.")      ; argument list
;;                 )
;;     (erl-receive ()
;; 	((['rex ['ok string]]
;; 	  (display-message-or-view string "*Expression Result*"))
;; 	 (['rex ['error reason]]
;; 	  (message "Error: %S" reason))
;; 	 (other
;; 	  (message "Unexpected: %S" other)))))

;; REFERENCES
;; 
;; http://bc.tech.coop/blog/070528.html

(defun sanitize-erlang-mode ()
  ;;(set-fill-column 78)
  (setq indent-tabs-mode nil)
  ;; add Erlang functions to an imenu menu
  ;;(imenu-add-to-menubar "imenu")
  (define-key erlang-mode-map (kbd "<return>") 'newline-and-indent)
  ;;(define-key erlang-mode-map (kbd "M-/") 'erl-complete)
  (define-key erlang-mode-map (kbd "M-?") 'erl-complete)
  (define-key erlang-mode-map (kbd "M-.") 'erl-find-source-under-point))

(defun sanitize-erlang-shell-mode ()
  ;;(define-key erlang-shell-mode-map (kbd "M-/") 'erl-complete)
  (define-key erlang-shell-mode-map (kbd "M-?") 'erl-complete)
  (define-key erlang-shell-mode-map (kbd "M-.") 'erl-find-source-under-point))

(defun erlang-setup-default-shell ()
  "Creates a new interactive erlang shell named 'emacs@yourhost'
which can easily be connected to by Distel.  If an erlang shell
has already been created, does nothing.

Bug: Will fail if two emacsen are open using an erlang shell."
  (interactive)
  (let ((oldopts inferior-erlang-machine-options))
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    ;; only create if it doesn't exist
    (if (null (get-buffer erlang-shell-buffer-name))
        (progn
          ;; make the shell but don't steal the focus
          (let ((curbuf (current-buffer)))
            (erlang-shell)
            (switch-to-buffer curbuf))
          ;; let distel know about this node and upload its wacky
          ;; modules so auto-completion can *just work*
          (let ((node-name (concat "emacs@" (erl-determine-hostname))))
            (setq erl-nodename-cache (intern node-name))
            (setq distel-modeline-node node-name)
            (force-mode-line-update)
            (sleep-for 2) ; give it a little time to init
            (erl-ping (intern node-name)))))
    (setq inferior-erlang-machine-options oldopts)))

(eval-after-load 'erlang
  (load "erlang_appwiz" t nil)
  (require 'erlang)
  (require 'erlang-start)
  (add-to-list 'load-path (concat dotfiles-dir "/vendor/distel/elisp"))
  (require 'distel)
  (distel-setup)
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))
  (add-hook 'erlang-mode-hook 'erlang-setup-default-shell)
  (add-hook 'erlang-mode-hook 'sanitize-erlang-mode)
  (add-hook 'erlang-shell-mode-hook 'sanitize-erlang-shell-mode))

;; Command line arguments for Erlang

;; Something like this is very convenient when working locally but you
;; will probably get errors if you try to create a shell on a remote
;; system because it will try to load sasl and stuff when it's already
;; running and give you name conflicts.

;; (setq inferior-erlang-machine-options '("-sname" "emacs"
;;                                         "-boot" "start_sasl"
;;                                         "-s" "reloader"))

;; ;; This is needed for Distel setup (do we really need this??)
;; (let ((distel-dir (concat dotfiles-dir "/vendor/distel/elisp")))
;;   (unless (member distel-dir load-path)
;;     ;; Add distel-dir to the end of load-path
;;     (setq load-path (append load-path (list distel-dir)))))

(provide 'starter-kit-erlang)
;;; starter-kit-erlang.el ends here

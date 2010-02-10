;;; lobstermacs-erc.el --- Make chatting on the IRC better

(defadvice erc-server-send (around lob/erc-more-rfc-compliant)
  ;; make erc not send an extra blank line when i chat
  (ad-set-arg 0 (replace-regexp-in-string "[\r\n]+$" "" (ad-get-arg 0)))
  ;; auto-join command should be capitalized
  (ad-set-arg 0 (replace-regexp-in-string "^join" "JOIN" (ad-get-arg 0)))
  ad-do-it)
(eval-after-load 'erc '(ad-activate 'erc-server-send))

;; openssl is much more widely installed than gnutls so we'll make
;; that the first choice to not waste time
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --priority secure256 -p %p %h"
                    "gnutls-cli --priority secure256 -p %p %h"))
;; come on people
(autoload 'erc-tls "erc" nil t)

(provide 'lobstermacs-erc)
;;; lobstermacs-erc.el ends here

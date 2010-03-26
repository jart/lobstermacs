;;;
;;
;; Plugin for browse-url to offer Google Chrome support

;; Support for opening webpages in Google Chrome
(defcustom browse-url-chrome-program "/opt/google/chrome/chrome"
  "The name by which to invoke Google Chrome."
  :type 'string
  :group 'browse-url)

(defun browse-url-chrome (url &optional new-window)
  "Simple way to open a url in your Google Chrome

You might need to customize the install path variable which is
stored inside `browse-url-chrome-program' (by default this is set
to: /opt/google/chrome/chrome)"
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
	 (process
	  (apply 'start-process
		 (concat "chrome " url) nil
		 browse-url-chrome-program
		 (list url))))
    process))

(provide 'lobstermacs-chrome)

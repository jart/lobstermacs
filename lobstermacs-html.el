;;; lobstermacs-html.el --- My special keybindings
;;
;; Part of Lobstermacs.

(eval-after-load 'mumamo
  '(progn
     ;; HOLY SHIT WHY IS MY HTML BLUE
     (custom-set-faces
      '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
      '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
      '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
      '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
      '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil))))))

(provide 'lobstermacs-html)
;;; lobstermacs-html.el ends here

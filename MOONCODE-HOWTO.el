
;; If you actually want to understand Emacs' crazy customization
;; language, this file might help you get a "feel" for how it works.

;; Press `C-x C-e` at the end of each of these lines to better
;; conceptual understanding of Moon Code.  Press `C-h f` to ask emacs
;; what a particular function does.  If that doesn't work try using
;; `C-h a` to search for a name.

;; Resources:
;;  http://steve-yegge.blogspot.com/2008_01_01_archive.html
;;  http://www.emacswiki.org/cgi-bin/emacs-en?CategoryCode
;;  http://cl-cookbook.sourceforge.net/strings.html

(setq debug-on-error t)
(message "hello kitty")
'(message "hello kitty")
(find-file "~/.bashrc")
(global-set-key (kbd "C-x C-x") 'doctor)
;; now press `C-x C-x`

'((apple . "red"))

2
"string"
(+ 2 2)
(- 5 2)
(+ 2 2 2)
(+ 2 (- 5 2))
(+ 2 (- 5 2) (- 5 2))
()

(list)
(list 3 4 1 5 2)
'(3 4 1 5 2)

'(hello there)
(quote (hello there))
'(hello there (+ 2 2))
(list 'hello 'there)
(list 'hello 'there (+ 2 2))
(sort (list 3 4 1 5 2) '<)
(sort (list "there" "hi") 'string-lessp)

'(+ 2 2 2)
(eval '(+ 2 2 2))
(apply '+ '(2 2 2))


;; user input
(defun asker (name)
  "this is my documentation for 'asker'"
  (interactive "sWhat's your name: ")
  (message (format "Thank you %s, have a lovely day :D" name)))
;; now type M-x asker
(if (y-or-n-p "Do it?")
    (do-something)
  (do-something-else))


;; working with the current buffer
buffer-file-name
(current-buffer)
(search-forward "system-name")
(search-backward "current [Bb]uffer")
(save-excursion
  (search-forward "system-name")
  (move-beginning-of-line nil)
  (insert ";; a new comment appears!\n"))

;; output data to a helpful buffer
(defun output-to-buffer (data &optional buffer-name)
  (let ((bufname (or buffer-name "*variable-output*")))
    (get-buffer-create bufname)
    (pop-to-buffer bufname))
  (end-of-buffer)
  (recenter)
  (save-excursion
    (insert data)))

(output-to-buffer (concat "\nYour load path:\n\n"
                          (mapconcat 'identity load-path "\n")))


;; detecting what mode you're in
major-mode
mode-name
minor-mode-alist
(equal "emacs-lisp-mode" (symbol-name major-mode))
;; sometimes modes like 'rst-mode' are derived from 'text-mode'
(derived-mode-class major-mode)


;; the fine line: variables/atoms, code/lists
system-name
system-type
system-configuration
system-configuration-options
'system-name
(atom 'system-name)
(stringp system-name)
(setq mooncode "MOONCODE")
'(setq mooncode "MOONCODE")
mooncode


(message system-name)
(concat "hello " "kitty")
(message (concat "hello " mooncode))
(format "hello")
(format "hello %d" 2)
(format "hello %05d" 2)
(format "hello %5d" 2)
(format "%20s %20s %20s %20s"
        "column 1" "column 2" "column 3" "column 4")
(format "ATTENTION\n\nOn %04d-%02d-%02d %s $%.2f to %s"
        2008 9 1
        "debit" 8.4 "sinners, inc.")
(progn
  (insert "hello ")
  (insert "new york"))
(progn
  (sleep-for 2) ; freezes screen :(
  "i'm back!")
(progn
  (sit-for 0 500)
  "i'm back!")

;; boolean logic
;; http://www.emacswiki.org/emacs-en/ComparisonFunctions
t
nil
(not t)
(not nil)
(and t t t)
(and t t nil)
(or t t nil)
(zerop 0) ; Return t if number is zero.
(string= "what a pain" "what a pain")
(string-lessp "hi" "there")
(string-lessp "there" "hi")
(eq 2 2)
(= 2 2)
(/= 2 3)
(> 5 2)
(< 5 2)
(<= 5 5)

;; language constructs
(if (eq (+ 2 2) 5)
    "compy is broken"
  "universe is safe")
(if (eq (+ 2 2) 4)
    (progn
      (+ 2 2)
      (+ 2 2)
      "i just did 3 things"))
(unless (eq (+ 2 2) 4)
    "compy is broken")
(unless (eq (+ 2 2) 4)
  (do-something))
;; case statement
(cond
 ((= (+ 2 2) 5) "this is so not true lol")
 ((= 2 2) "happiness")
 (t "all other conditions returned nil"))

;; try/FINALLY clause:
(unwind-protect
    (error "hello")
  (insert "hi"))

;; try/EXCEPT clause:
(condition-case ex
    (error "hello")
  ('error (message (format "omg: [%s]" ex))))
(condition-case ex
  ('error (message (format "omg: [%s]" ex))))

;; while loop
(setq i 0)
(while (< i 10)
  (insert "oh snap ")
  (setq i (+ i 1)))

;; foreach loop
(mapcar '(lambda (x) (concat "hi: " x)) (list "omg" "lol"))
(mapcar '(lambda (x) (concat "hi: " x)) (list "omg" "lol"))
(mapcar 'message load-path)

(message "Hello")

;; for loop
(progn
  (let ((misery "I have haxored into your computer")
        (count  (length misery))
        (pause  100))
    (dotimes (i count)
      (sit-for 0 pause)
      (insert (substring misery i (+ i 1))))))

;; strings
;; Do M-x apropos RET \bstring\b RET to see a list of functions related to strings.
(substring "hello" 0 1)
(substring "hello" 1 2)
(string-match "h" "hello")
(string-match "ell" "hello")
;; this uses regular expressions so be careful
(string-match "o$" "hello")
(string-match "ll$" "hello")
(string-match "CODE" (buffer-name (current-buffer)))
(replace-regexp-in-string " " "-" "is this the place i used to call fatherland")
(split-string "war,is,peace" ",")
(downcase "OMG what you know about Emacs dawg?")
(upcase "OMG what you know about Emacs dawg?")
(capitalize "bob dole")
;; join list of strings
(mapconcat 'identity (list "hello" "little" "kitty") "__")
(split-string "war,is,peace" ",")

;; type checking
(null nil)
(not (null nil))
(null "hehe")
(stringp "hehe")
(listp '(+ 2 2))
(listp '(+ 2 2))
(arrayp [what is this])
(arrayp "a string i guess")
(atom 'system-name)
(atom 666)
;; is function declared?
(fboundp 'message)

;; type-casting
(intern "hello")      ;; string -> atom
(make-symbol "hello") ;; string -> atom
(symbol-name 'hi)     ;; atom -> string
(eval `(,(intern "message") "take THAT moon code"))
(number-to-string 256)
(string-to-number "256")


;; make your own functions
(defun adding_up_numbers (x y)
  (+ x y))
(adding_up_numbers 2 2)
(defun adding_up_numbers (x y)
  (+ x y)
  "omg")
(adding_up_numbers 2 2)
(adding_up_numbers 2 4)


;; make your own command that you can run with `M-x`
(defun my_own_command ()
  (interactive)
  (message "Hello Kitty!"))
;; now type M-x my_own_command



;; lambda calculus or something (type lam-bda without to make that symbol
;; appear)
(eval "hello")
(eval '(message "hello"))
(funcall 'message "hello")
(lambda () "hello")
(funcall (lambda () "hello"))
(setq my-func (lambda () "hello"))
(funcall my-func)
(funcall (lambda (arg1)
           "hello "))

(global-set-key (kbd "C-x C-x") 'doctor)
(global-set-key (kbd "C-x C-x") (lambda ()
                                  (interactive)
                                  (doctor)
                                  (message "omg you're so crazy")))


;; working with files
(file-exists-p "/tmp")
(expand-file-name "../../../etc/passwd")
(file-name-extension "/the/dir/filez.pdf")


;; running shell commands
(executable-find "ispell")
(shell-command-to-string "ls")
(shell-command "ls")
(progn (shell-command "ls -al" "*ls-results-buf*")
       (switch-to-buffer-other-window "*ls-results-buf*")
       (end-of-buffer)
       (insert "\n---------------\n")
       (insert "Enjoy your command results!!\n"))


;; working with lists like arrays
(length (list 1 2 3))
(nth 0 (list 6 7 8))
(nth 2 (list 6 7 8))
;; is element in list?
(memq 'there (list 'hi 'there))
(memq 'dog (list 'hi 'there))
(memq system-type '(ms-dos windows-nt))
(memq system-type '(ms-dos windows-nt))
;; head of list
(car '(+ 2 2))
(car '())
;; tail of list
(cdr '(+ 2 2))
(cdr '())
(cdr '(2))
;; omg a name that *almost* makes sense
(last '(2 3 4 5 6))
(butlast '(2 3 4 5 6))
;; put them back together
(cons '+ '(2 2))
;; glue two lists together
(append (list 1 2) (list 3 4))
;; have fun
(cons "life"
      (cons "is"
            (list "too" "long")))
(setq mylist (list 6 7 8))
(pop mylist)
(push 6 mylist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVANCED MOON CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backtics are generally useful for generating code
(list 'format "your system: %s" 'system-name)
'(format "your system: %s" system-name)
`(format "your system: %s" system-name)
(eval `(format "your system: %s" system-name))
`(format "your system: %s" ,system-name)
(eval `(format "your system: %s" ,system-name))

(defmacro 5x (codez)
  `(progn
     ,codez
     ,codez
     ,codez
     ,codez
     ,codez))

(5x
 (insert "hello "))

(macroexpand
 '(5x
   (insert "hello ")))

(defmacro nevar-fail (primary failover)
  "Runs primary code.  If primary code fails, then executes
  failover code."
  `(condition-case exc
       ,primary
     ('error
      (message (format "Caught exception: [%s]" exc))
      ,failover)))

(nevar-fail (message "hello") (message "backup code"))
(nevar-fail (error "omg") (message "backup code"))

`(progn
   (message (+ 2 2))
   (message ,(+ 2 2))
   (message ,system-name)
   (message ,system-name))

`(hello there ,@'(2 3 4))
`(hello there ,@(list 2 3 4))

[2 3 4]
(vector 2 3 4)
[2 3 (+ 2 3)]
[2 3 (eval '(+ 2 3))]

(sort (list 2 3 4 5) '<)
;; sum

(reduce #'+ (list 2 3 4 5))
(reduce '+ (list 2 3 4 5) :initial-value 20)

;; map (doubler)
(mapcar (lambda (x) (* x 2)) (list 2 3 4 5))

;; flatten list
(nconc (list  "hi" "there") (list  "yo" "yo"))
(mapcar (lambda (x) (list "-f" x)) (list "file1" "file2" "file3"))
(mapcan (lambda (x) (list "-f" x)) (list "file1" "file2" "file3"))
(eval (cons 'concat (mapcan (lambda (x) (list "-f" x)) (list "file1" "file2" "file3"))))
(mapconcat (lambda (x) (concat "-f " x)) (list "file1" "file2" "file3") " ")


(defun factorial (x)
  (if (zerop x)
      1
    (* x (factorial (- x 1)))))
;; no tail-recursion so you won't get much higher than this
(factorial 5)

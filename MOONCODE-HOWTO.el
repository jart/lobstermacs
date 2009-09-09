
;; If you actually want to understand Emacs' crazy customization
;; language, this file might help you.

;; Press `C-x C-e` at the end of each of these lines to better
;; conceptual understanding of Moon Code.  Press `C-h f` to ask emacs
;; what a particular function does.

(message "hello kitty")
(global-set-key (kbd "C-x C-x") 'doctor)

"string"
(+ 2 2)
(- 5 2)
(+ 2 2 2)
(+ 2 (- 5 2))
(+ 2 (- 5 2) (- 5 2))

(list)
(list 3 4 1 5 2)
'(3 4 1 5 2)

'(hello there)
'(hello there (+ 2 2))
(list 'hello 'there)
(list 'hello 'there (+ 2 2))
(sort (list 3 4 1 5 2))

'(+ 2 2)
(eval '(+ 2 2))

(map atom '+ (list 2 3 4 5))

;; symbols
system-name
'system-name
(setq mooncode "MOONCODE")
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
t
nil
(not t)
(not nil)
(and t t t)
(and t t nil)
(or t t nil)
(zerop 0) ; Return t if number is zero.
(string= "what a pain" "what a pain")
(eq 2 2)
(> 5 2)
(< 5 2)
(<= 5 5)

;; language constructs
(if (eq (+ 2 2) 5)
    "compy is broken"
  "universe is safe")
(unless (eq (+ 2 2) 4)
    "compy is broken")
(unless (eq (+ 2 2) 4)
  (do-something))

(setq i 0)
(while (< i 10)
  (insert "oh snap ")
  (setq i (+ i 1)))

(progn
  (let ((misery "I have haxored into your computer")
        (count  (length misery))
        (pause  100))
    (dotimes (i count)
      (sit-for 0 pause)
      (insert (substring misery i (+ i 1))))))


;; strings
(substring "hello" 0 1)
(substring "hello" 1 2)
(split-string "war,is,peace" ",")
(downcase "OMG what you know about Emacs dawg?")
(upcase "OMG what you know about Emacs dawg?")
(capitalize "bob dole")


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


(length (list 1 2 3))
(nth 0 (list 6 7 8))
(nth 2 (list 6 7 8))
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

`(hello there (+ 2 2))
`(hello there ,(+ 2 2))
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

(defun factorial (x)
  (if (zerop x)
      1
    (* x (factorial (- x 1)))))
;; no tail-recursion so you won't get much higher than this
(factorial 5)

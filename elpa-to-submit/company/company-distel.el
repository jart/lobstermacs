;;; company-distel.el --- a company-mode completion back-end for Erlang
;;
;; Copyright (C) 2009 J.A. Roberts Tunney
;;
;; This file is NOT part of company
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))
(require 'erlang)
(require 'distel)

;; OMG how distel is all asynchronous and stuff, i'm having a hard
;; time making my message passing "block" so an s-expr return a list
;; of completions.

(defun company-distel-get-completions ()
  "This function has some serious kludging.  The loop is
   technically infinite if there are nil completions but it seems
   `sit-for` will interrupt if the user presses a key."
  (setq mop nil)
  (erl-spawn
    (erl-send-rpc (erl-target-node) 'distel 'modules (list "crashdum"))
    (erl-receive ()
        ((['rex ['ok completions]]
          (setq mop completions))
         (['rex ['error reason]]
          (message "Error: %s" reason))
         (other
          (message "Unexpected reply: %S" other)))))
  (while (null mop) (sit-for 0 200))
  mop)

;; (let (mop nil)
;;   (erl-send-rpc (erl-target-node) 'distel 'modules (list "crashdum"))
;;   (sit-for 0 500)
;;   (pop erl-mailbox))

;;;###autoload
(defun company-distel (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for distel.
This requires Erlang OTP, erlang-mode, and that you've compiled
the Distel enhancements to erlang-mode."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-distel))
    ('prefix "crashdum")
    ('candidates (company-distel-get-completions))))

(provide 'company-distel)
;;; company-distel.el ends here

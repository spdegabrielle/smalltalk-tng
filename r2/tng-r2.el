;;; tng-r2.el --- ThiNG r2 code editing commands for Emacs

;; Add code like the following to your .emacs to install:
;; (autoload 'tng-r2-mode "...path.to.wherever.you.put.this.file.../tng-r2.el" nil t)
;; (setq auto-mode-alist (cons '("\\.tng\\'" . tng-r2-mode)
;;                             auto-mode-alist))

;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.
;; Copyright (C) 2003, 2005 Tony Garnock-Jones <tonyg@lshift.net>

;; This file is based on GNU Emacs' AWK mode (awk-mode.el).

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'cc-mode)

(defvar tng-r2-mode-syntax-table nil
  "Syntax table in use in tng-r2-mode buffers.")

(if tng-r2-mode-syntax-table
    ()
  (setq tng-r2-mode-syntax-table (make-syntax-table))
  (cond ((memq '8-bit c-emacs-features) ;; XEmacs
	 (modify-syntax-entry ?\" "! " tng-r2-mode-syntax-table))
	((memq '1-bit c-emacs-features) ;; Proper Emacs
	 (modify-syntax-entry ?\" "! " tng-r2-mode-syntax-table)))
  (modify-syntax-entry ?_ "_" tng-r2-mode-syntax-table)
  (mapcar #'(lambda (x) (modify-syntax-entry x "." tng-r2-mode-syntax-table))
	  '(?, ?\; ?- ?+ ?= ?| ?/ ?? ?. ?< ?> ?* ?& ?^ ?% ?$ ?# ?@ ?! ?` ?~))
  (modify-syntax-entry ?\' "\"" tng-r2-mode-syntax-table))

(defconst tng-r2-font-lock-keywords
  (eval-when-compile
    (list
     ;; Keywords.
     '("\\<define\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\)*\\>" . font-lock-keyword-face)
     '("\\<\\(new\\)\\s \\([A-Z]\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\|::\\)*\\)\\>"
       (1 font-lock-keyword-face) (2 font-lock-type-face))
     (regexp-opt
      '(
	"define"
	"in"
	"let"
	"letrec"
	"module"
	"new"
	)
      'words)

     ;; Namespaces.
     '("\\<[A-Z]\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\)*\\>::" . font-lock-type-face)

     ;; Atoms.
     '("\\<[A-Z]\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\)*\\>" . font-lock-constant-face)

     ;; Variables.
     '("\\<[a-z]\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\)*\\>" . font-lock-variable-name-face)

     ;; Infixops.
     '("\\<[-+=_|/?.<>*&^%$#@!`~]\\([-a-zA-Z0-9+=_|/?.<>*&^%$#@!`~]\\)*\\>"
       . font-lock-function-name-face)
     ))
 "Default expressions to highlight in TNG-R2 mode.")

;;;###autoload
(define-derived-mode tng-r2-mode c-mode "TNG-R2"
  "Major mode for editing TNG-R2 code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on TNG-R2 mode runs `tng-r2-mode-hook'."
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "\"")
  (setq comment-end "\"")
  (setq comment-start-skip "\" *")
  (make-local-variable 'c-syntactic-indentation)
  (setq c-syntactic-indentation nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(tng-r2-font-lock-keywords nil nil ((?_ . "w")))))

(provide 'tng-r2-mode)

;;; tng-r2.el ends here

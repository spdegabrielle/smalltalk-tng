;;; tng-r3.el --- ThiNG r3 code editing commands for Emacs

;; Add code like the following to your .emacs to install:
;; (autoload 'tng-r3-mode "...path.to.wherever.you.put.this.file.../tng-r3.el" nil t)
;; (setq auto-mode-alist (cons '("\\.tng\\'" . tng-r3-mode)
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

(require 'cmuscheme)
(require 'comint)
(require 'cc-mode)

(defvar tng-r3-mode-syntax-table nil
  "Syntax table in use in tng-r3-mode buffers.")

(if tng-r3-mode-syntax-table
    ()
  (setq tng-r3-mode-syntax-table (make-syntax-table))
  (cond ((memq '8-bit c-emacs-features) ;; XEmacs
	 (modify-syntax-entry ?\" "! " tng-r3-mode-syntax-table))
	((memq '1-bit c-emacs-features) ;; Proper Emacs
	 (modify-syntax-entry ?\" "! " tng-r3-mode-syntax-table)))
  (modify-syntax-entry ?_ "_" tng-r3-mode-syntax-table)
  (mapcar #'(lambda (x) (modify-syntax-entry x "." tng-r3-mode-syntax-table))
	  '(?, ?\; ?- ?+ ?= ?| ?/ ?? ?. ?< ?> ?* ?& ?^ ?% ?$ ?# ?@ ?! ?` ?~))
  (modify-syntax-entry ?\' "\"" tng-r3-mode-syntax-table))

(defconst tng-r3-font-lock-keywords
  (eval-when-compile
    (list
     '(";;" . font-lock-warning-face)

     ;; Atoms.
     '("\\.\\<\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>" . font-lock-constant-face)

     ;; Keywords.
     '("\\<define\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>" . font-lock-keyword-face)
     '("\\<\\(new\\)\\s \\([a-zA-Z]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\|::\\)*\\)\\>"
       (1 font-lock-keyword-face) (2 font-lock-type-face))
     (regexp-opt
      '(
	"define"
	"in"
	"let"
	"letrec"
	"module"
	"new"
	"return"
	)
      'words)

     ;; Namespaces.
     '("\\<[A-Z]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>::" . font-lock-type-face)

     ;; Types.
     '("\\<[A-Z]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>" . font-lock-type-face)

     ;; Functions.
     '("(\\(\\<[a-zA-Z]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>\\)\\($\\|\\s [^-+=_|/?<>*&^%$@!`~#]\\)"
       (1 font-lock-function-name-face))

     ;; Variables.
     '("\\<[a-zA-Z]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>" . font-lock-variable-name-face)

     ;; Infixops.
     '("\\<[-+=_|/?<>*&^%$@!`~]\\([-a-zA-Z0-9+=_|/?<>*&^%$@!`~]\\)*\\>"
       . font-lock-function-name-face)
     ))
 "Default expressions to highlight in TNG-R3 mode.")

(defun tng-mode-variables ()
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "\"")
  (setq comment-end "\"")
  (setq comment-start-skip "\" *")
  (make-local-variable 'c-syntactic-indentation)
  (setq c-syntactic-indentation nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(tng-r3-font-lock-keywords nil nil ((?_ . "w")))))

;;;###autoload
(define-derived-mode tng-r3-mode c-mode "TNG-R3"
  "Major mode for editing TNG-R3 code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on TNG-R3 mode runs `tng-r3-mode-hook'." ;; actually a lie
  (tng-mode-variables))

;---------------------------------------------------------------------------

(defcustom tng-program-name "./main.scm"
  "*Program invoked by the `run-tng' command."
  :type 'string
  :group 'tng)

(defcustom inferior-tng-mode-hook nil
  "*Hook for customising inferior-tng mode."
  :type 'hook
  :group 'tng)

(defvar tng-buffer)

(define-derived-mode inferior-tng-mode comint-mode "Inferior ThiNG"
  "Major mode for interacting with an inferior ThiNG process."
  ;; Customise in inferior-tng-mode-hook
  (setq comint-prompt-regexp "^\"[^\"\n]*\" *")
  (tng-mode-variables)
  (setq mode-line-process '(":%s")))

;;;###autoload
(defun run-tng (cmd)
  "Run an inferior ThiNG process, input and output via buffer *tng*.
If there is a process already running in `*tng*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `tng-program-name').  Runs the hooks `inferior-tng-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run ThiNG: " tng-program-name)
		       tng-program-name)))
  (if (not (comint-check-proc "*tng*"))
      (let ((cmdlist (scheme-args-to-list cmd)))
	(set-buffer (apply 'make-comint "tng" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-tng-mode)))
  (setq tng-program-name cmd)
  (setq tng-buffer "*tng*")
  (pop-to-buffer "*tng*"))

(provide 'tng-r3-mode)

;;; tng-r3.el ends here

(defconst tng2-mode-file-extension-regexp "\\.tng2\\'")
(let ((cell (assoc tng2-mode-file-extension-regexp auto-mode-alist)))
  (if cell
      (rplacd cell 'tng2-mode)
    (setq auto-mode-alist (cons `(,tng2-mode-file-extension-regexp . tng2-mode) auto-mode-alist))))

(defun tng2-mode ()
  "Major mode for editing TNG2 programs (tonyg)."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tng2-mode)
  (setq mode-name "Smalltalk-TNG Experimental Syntax 2 Mode")
  (set-syntax-table tng2-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(tng2-font-lock-keywords nil nil () ()))
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (let ((x '()))
			(dotimes (i 60) (push (* 2 (+ i 1)) x))
			(reverse x)))
  (font-lock-fontify-buffer))

(defconst tng2-font-lock-keywords
  '(
;    ("\\<%%primitive\\.[0-9]+\\.[0-9]+" . font-lock-function-name-face)
;    ("\\<%%machine-code\\>" . font-lock-warning-face)
;    ("\\<define\\s-+\\(\\(\\w\\|\\s_\\)+\\)\\>" 1 font-lock-variable-name-face)
    ("\\<\\([A-Z]\\w*\\)\\>" 0 font-lock-type-face)
    ("\\<\\(\\(\\w\\|\\s_\\)+:\\)" 1 font-lock-function-name-face)
    ("\\$" 0 font-lock-function-name-face)
    ("!\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\>" 1 font-lock-variable-name-face)
    ("\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-+:=" 1 font-lock-variable-name-face)
    ("\\<\\(meta\\)\\>" 1 font-lock-keyword-face)
   ))

(defvar tng2-mode-syntax-table nil
  "Syntax table used while in TNG2 mode.")

(if tng2-mode-syntax-table
    ()
  (setq tng2-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"   " tng2-mode-syntax-table)
  (modify-syntax-entry ?\" "!   " tng2-mode-syntax-table)
  (modify-syntax-entry ?? "_ " tng2-mode-syntax-table)
  (modify-syntax-entry ?- "_ " tng2-mode-syntax-table)
  (modify-syntax-entry ?: "_ " tng2-mode-syntax-table)
  (modify-syntax-entry ?\( "()" tng2-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" tng2-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" tng2-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" tng2-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" tng2-mode-syntax-table)
  (modify-syntax-entry ?\} "){" tng2-mode-syntax-table)
)

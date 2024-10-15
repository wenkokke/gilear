;;; gilear-mode.el --- major mode for gilear  -*- lexical-binding: t; -*-

;; The haskell-mode-syntax-table is a useful guide:
;; https://github.com/haskell/haskell-mode/blob/1a285fc4c50ca74bb5cd9b2a8c1a46a64a77384a/haskell-mode.el#L448
(defvar gilear-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; The "(" character is an opening parenthesis closed by ")".
    (modify-syntax-entry ?\( "()" table)
    ;; The ")" character is an closing parenthesis opened by "(".
    (modify-syntax-entry ?\) ")(" table)
    ;; The "{" character is an opening parenthesis closed by "}",
    ;; and it is the first character in the b-style two-character
    ;; comment opener which can be nested.
    (modify-syntax-entry ?\{ "(}1nb" table)
    ;; The "}" character is an closing parenthesis opened by "{",
    ;; and it is the last character in the b-style two-character
    ;; comment closer which can be nested.
    (modify-syntax-entry ?\} "){4nb" table)
    ;; The "-" character is the character in a-style the two-character
    ;; comment opener which is closed by a newline, it is the last
    ;; character in the b-style two-character comment opener, and it is
    ;; the first character in the b-style two-charater comment closer.
    (modify-syntax-entry ?- "< 123" table)
    ;; The newline is the a-style comment closer.
    (modify-syntax-entry ?\n  ">" table)
    table)
  "Syntax table used in `gilear-mode'.")

(defvar gilear-mode-abbrev-table nil
  "Abbreviation table used in `gilear-mode' buffers.")

(define-abbrev-table 'gilear-mode-abbrev-table
  '())

;;;###autoload
(define-derived-mode gilear-mode prog-mode "Gilear"
  "Major mode for editing Gilear programs."
  :abbrev-table gilear-mode-abbrev-table
  (setq-local comment-start "--")
  (setq-local comment-padding 1)
  (setq-local comment-start-skip "[-{]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-}\\|\\s>\\)")
  )

;; Provide gilear-mode:
(provide 'gilear-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gilear" . gilear-mode))

;;; gilear-mode.el ends here

;;; poule-mode.el --- Major mode for the Poule proof language -*- lexical-binding: t; -*-

;; Author: OpenAI
;; Version: 0.1
;; Keywords: languages, proofs
;; Package-Requires: ((emacs "26.1"))
;;
;; This file provides a lightweight major mode for editing Poule sources.
;; It includes:
;; - syntax highlighting
;; - OCaml-style comments: (* ... *)
;; - a simple indentation engine
;; - imenu support
;;
;; Save this file as poule-mode.el and load it from Emacs.

;;; Code:

(require 'prog-mode)
(require 'rx)
(require 'imenu)

(defgroup poule nil
  "Major mode for the Poule proof language."
  :group 'languages)

(defcustom poule-indent-offset 2
  "Indentation width for `poule-mode'."
  :type 'integer
  :group 'poule)

(defconst poule--keywords
  '("axiom" "by" "check" "compute" "corollary" "def" "else" "end"
    "exists" "fail" "fix" "forall" "fun" "if" "in" "inductive"
    "lemma" "let" "match" "module" "mutual" "notation" "of"
    "print" "proof" "qed" "rec" "refine" "rewrite" "show"
    "sig" "solve" "sort" "structure" "tactic" "theorem" "then"
    "type" "undo" "use" "where" "with"))

(defconst poule--builtin-tactics
  '("admit" "apply" "assumption" "constructor" "contradiction"
    "destruct" "eapply" "exact" "exists" "induction" "intros"
    "left" "reflexivity" "rename" "repeat" "replace" "revert"
    "right" "rewrite" "simpl" "split" "subst" "symmetry"
    "transitivity" "trivial" "unfold"))

(defconst poule--constants
  '("False" "True" "Prop" "Set" "Type"))

(defconst poule--font-lock-keywords
  (let* ((kw (regexp-opt poule--keywords 'symbols))
         (tac (regexp-opt poule--builtin-tactics 'symbols))
         (const (regexp-opt poule--constants 'symbols))
         (lower-ident "\\_<[a-z_][A-Za-z0-9_']*\\_>")
         (upper-ident "\\_<[A-Z][A-Za-z0-9_']*\\_>")
         (tyvar "\\_<'+[A-Za-z_][A-Za-z0-9_']*\\_>"))
    `(
      (,kw . font-lock-keyword-face)
      (,tac . font-lock-builtin-face)
      (,const . font-lock-constant-face)

      ;; Type variables: 'a, 'state, etc.
      (,tyvar . font-lock-type-face)

      ;; Constructors / module-like names.
      (,upper-ident . font-lock-type-face)

      ;; Declaration heads after key language forms.
      (,(rx symbol-start
            (or "theorem" "lemma" "axiom" "def" "let" "inductive" "type"
                "module" "notation" "corollary")
            symbol-end
            (+ space)
            (group (+ (or word ?_ ?'))))
       1 font-lock-function-name-face)

      ;; Bound variable after forall / exists / fun / fix.
      (,(rx symbol-start
            (or "forall" "exists" "fun" "fix")
            symbol-end
            (+ space)
            (group (+ (or word ?_ ?'))))
       1 font-lock-variable-name-face)

      ;; Match branch variable names are usually lowercase identifiers.
      (,lower-ident . font-lock-variable-name-face))))

(defvar poule-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; OCaml-style comments: (* ... *)
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\) ")(4n" st)

    ;; Treat underscore and apostrophe as part of identifiers.
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)

    ;; Common symbolic operators.
    (dolist (ch '(?= ?< ?> ?+ ?- ?/ ?* ?| ?& ?: ?! ?? ?% ?^ ?.))
      (modify-syntax-entry ch "." st))
    st)
  "Syntax table for `poule-mode'.")

(defconst poule--block-open-regexp
  (rx line-start
      (* space)
      (or "proof" "match" "with" "then" "else"
          "let" "fun" "forall" "exists" "fix"
          "module" "sig" "structure" "inductive" "type")
      symbol-end))

(defconst poule--block-middle-regexp
  (rx line-start
      (* space)
      (or "|" "with" "else")
      symbol-end))

(defconst poule--block-close-regexp
  (rx line-start
      (* space)
      (or "end" "qed")
      symbol-end))

(defun poule--previous-significant-line ()
  "Move point to previous non-empty, non-comment-only line.
Return point, or nil if not found."
  (catch 'done
    (while (zerop (forward-line -1))
      (beginning-of-line)
      (cond
       ((looking-at-p "[ \t]*$"))
       ((looking-at-p "[ \t]*(\\*"))
       (t (throw 'done (point)))))
    nil))

(defun poule--line-starts-with-close-p ()
  "Return non-nil if current line starts with a closing token."
  (save-excursion
    (back-to-indentation)
    (or (looking-at-p poule--block-close-regexp)
        (looking-at-p "|")
        (looking-at-p ")")
        (looking-at-p "]")
        (looking-at-p "}"))))

(defun poule--previous-line-indent ()
  "Return indentation of previous significant line."
  (save-excursion
    (if (poule--previous-significant-line)
        (current-indentation)
      0)))

(defun poule--previous-line-opens-block-p ()
  "Return non-nil if previous significant line opens a block."
  (save-excursion
    (when (poule--previous-significant-line)
      (back-to-indentation)
      (or
       (looking-at-p poule--block-open-regexp)
       (looking-at-p ".*\\_<\\(=\\|->\\)\\_>[ \t]*$")
       (looking-at-p ".*([ \t]*$")
       (looking-at-p ".*\\[[ \t]*$")
       (looking-at-p ".*{[ \t]*$")))))

(defun poule--calculate-indentation ()
  "Compute indentation for current line."
  (let ((base (poule--previous-line-indent)))
    (cond
     ;; Top of buffer.
     ((bobp) 0)

     ;; Closing tokens outdent.
     ((poule--line-starts-with-close-p)
      (max 0 (- base poule-indent-offset)))

     ;; Branch separators align one level inside enclosing block.
     ((save-excursion
        (back-to-indentation)
        (looking-at-p "|"))
      (max 0 (- base
                (if (save-excursion
                      (poule--previous-significant-line)
                      (back-to-indentation)
                      (looking-at-p "|"))
                    0
                  poule-indent-offset))))

     ;; Continue after an opening line.
     ((poule--previous-line-opens-block-p)
      (+ base poule-indent-offset))

     (t base))))

(defun poule-indent-line ()
  "Indent current line for `poule-mode'."
  (interactive)
  (let ((target (poule--calculate-indentation))
        (offset (- (current-column) (current-indentation))))
    (indent-line-to target)
    (when (> offset 0)
      (move-to-column (+ target offset)))))

(defvar poule-imenu-generic-expression
  `(("Theorems"
     ,(rx line-start (* space)
          (or "theorem" "lemma" "corollary" "axiom")
          (+ space)
          (group (+ (or word ?_ ?'))))
     1)
    ("Definitions"
     ,(rx line-start (* space)
          (or "def" "let" "fix")
          (+ space)
          (group (+ (or word ?_ ?'))))
     1)
    ("Types"
     ,(rx line-start (* space)
          (or "type" "inductive")
          (+ space)
          (group (+ (or word ?_ ?'))))
     1)
    ("Modules"
     ,(rx line-start (* space)
          (or "module" "sig" "structure")
          (+ space)
          (group (+ (or word ?_ ?'))))
     1)))

;;;###autoload
(define-derived-mode poule-mode prog-mode "Poule"
  "Major mode for editing the Poule proof language."
  :syntax-table poule-mode-syntax-table
  (setq-local font-lock-defaults '(poule--font-lock-keywords))
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\*+)")
  (setq-local indent-line-function #'poule-indent-line)
  (setq-local imenu-generic-expression poule-imenu-generic-expression)
  (setq-local electric-indent-chars
              (append "{}[]()|". electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.poule\\'" . poule-mode))

(provide 'poule-mode)

;;; poule-mode.el ends here

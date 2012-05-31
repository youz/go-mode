;;; -*- mode:lisp;package:editor -*-

;;; go-mode for xyzzy

;;; original
;;; http://code.google.com/p/go/source/browse/misc/emacs/go-mode.el


;;; Code:

(in-package :editor)
(export '(go-mode
	  gofmt
	  *go-mode-map*
	  *go-mode-hook*
	  *go-mode-tab-width*
	  *go-mode-function-name-style*
	  *go-mode-type-style*
	  *go-mode-constant-style*
	  *go-mode-keyword-file*
	  *godoc-command*
	  ))

(defvar *godoc-command* "godoc.exe")

(defvar *go-mode-syntax-table*
  (let ((st (make-syntax-table)))
    ;; Add _ to :word: character class
    (set-syntax-symbol st #\_)
    (set-syntax-escape st #\\)
    (set-syntax-match st #\( #\))
    (set-syntax-match st #\{ #\})
    (set-syntax-match st #\[ #\])
    (set-syntax-string st #\")

    ;; Operators (punctuation)
    (set-syntax-punctuation st #\+)
    (set-syntax-punctuation st #\+)
    (set-syntax-punctuation st #\-)
    (set-syntax-punctuation st #\*)
    (set-syntax-punctuation st #\/)
    (set-syntax-punctuation st #\%)
    (set-syntax-punctuation st #\&)
    (set-syntax-punctuation st #\|)
    (set-syntax-punctuation st #\^)
    (set-syntax-punctuation st #\!)
    (set-syntax-punctuation st #\=)
    (set-syntax-punctuation st #\<)
    (set-syntax-punctuation st #\>)
    (set-syntax-punctuation st #\')
    (set-syntax-punctuation st #\`)

    (set-syntax-start-c++-comment st #\/)
    (set-syntax-end-c++-comment st #\LFD)
    (set-syntax-start-multi-comment st "/*")
    (set-syntax-end-multi-comment st "*/")

    st)
  "Syntax table for Go mode.")

(defvar *go-mode-keyword-file* "~/etc/Go")
(defvar *go-mode-keyword-hash-table* nil)

(defvar *go-mode-keywords*
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var")
  "All keywords in the Go language.  Used for font locking and
some syntax analysis.")

(defvar *go-mode-function-name-style*
  '(:color 12))

(defvar *go-mode-type-style*
  '(:color 10))

(defvar *go-mode-constant-style*
  '(:keyword 2))

(defun go-mode-regexp-keyword-list ()
  "Basic font lock keywords for Go mode.  Highlights keywords,
built-ins, functions, and some types."
  (let ((type-name "\\s *\\(?:[*(]\\s *\\)*\\(?:\\w+\\s *\\.\\s *\\)?\\(\\w+\\)"))
    (compile-regexp-keyword-list
     `(;("\\<func\\>\\s *\\(\\w+\\)" t ,*go-mode-function-name-style* nil 1 1)
       ;; Function names in methods are handled by function call pattern
       ;; Function names in calls
       ;; XXX Doesn't match if function name is surrounded by parens
       ("\\(\\_s+\\)\\s *(" t ,*go-mode-function-name-style* nil 1 1)
       ;; Type names
       ;("\\<type\\>\\s *\\(\\w+\\)" t ,*go-mode-type-style* nil 1 1)
       ;       (,(concat "\\<type\\>\\s *\\w+\\s *" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; Arrays/slices/map value type
       ;; XXX Wrong.  Marks 0 in expression "foo[0] * x"
       ;;      (,(concat "]" type-name) 1 font-lock-type-face)
       ;; Map key type
       (,(concat "\\<map\\s *\\[" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; Channel value type
       (,(concat "\\<chan\\>\\s *\\(?:<-\\)?" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; new/make type
       (,(concat "\\<\\(?:new\\|make\\)\\>\\(?:\\s \\|)\\)*(" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; Type conversion
       (,(concat "\\.\\s *(" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; Method receiver type
       ;(,(concat "\\<func\\>\\s *(\\w+\\s +" type-name) t ,*go-mode-type-style* nil 1 1)
       ;; Labels
       ;; XXX Not quite right.  Also marks compound literal fields.
       ("^\\s *\\(\\w+\\)\\s *:\\(\\S.\\|$\\)" t ,*go-mode-constant-style* nil 1 1)
       ("\\<\\(goto\\|break\\|continue\\)\\>\\s *\\(\\w+\\)" t ,*go-mode-constant-style* nil 2 2)))))


(defvar *go-mode-tab-width* 4)
(defvar *go-mode-hook* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key map
;;

(defvar *go-mode-map*
  (let ((m (make-sparse-keymap)))
    (define-key m #\} 'go-mode-electric-close)
    (define-key m #\) 'go-mode-electric-close)
    (define-key m #\, 'go-mode-electric-insert)
;    (define-key m #\: 'go-mode-delayed-electric)

    ;; In case we get : indentation wrong, correct ourselves
    (define-key m #\= 'go-mode-electric-insert)
    (define-key m #\TAB 'go-mode-indent-line)
    (define-key m #\RET 'go-mode-newline-and-indent)
    (define-key m '(#\C-c #\l) 'list-function)
    (define-key m #\F1 'godoc)
    (define-key m '(#\C-c #\h) 'godoc)
    m)
  "Keymap used by Go mode to implement electric keys.")

(defun go-mode-electric-insert (&optional (arg 1))
  "Invoke the global binding of KEY, then reindent the line."

  (interactive)
  (unless (prog1 (parse-point-syntax)
	    (self-insert-command arg))
    (go-mode-indent-line)))

(defun go-mode-electric-close (&optional (arg 1))
  (interactive "*p")
  (unless (prog1
	      (parse-point-syntax)
	    (self-insert-command arg))
    (go-mode-indent-line))
  (save-excursion
    (forward-char -1)
    (and (goto-matched-parenthesis)
	 (show-matched-parenthesis)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;

(defun go-mode-cs (&optional (pos (point)) getend)
  (case (parse-point-syntax pos)
    (:string
     (values :string (go-mode-in-string pos)))
    (:comment
     (values :comment (go-mode-in-comment pos)))
    (t nil)))

(defun go-mode-in-string (pos)
  (when #0=(eq (parse-point-syntax pos) :string)
    (save-excursion
      (goto-char pos)
      (while #0#
	(skip-syntax-spec-backward "^\"")
	(backward-char))
      (point))))

(defun go-mode-in-comment (&optional (pos (point)) getend)
  (when #0=(eq (parse-point-syntax) :comment)
    (save-excursion
      (goto-char pos)
      (while (scan-buffer "/\\(/\\|\\*\\)" :regexp t :reverse t :no-dup t)
	(when (not #0#)
	  (return-from go-mode-in-comment (point)))))))


(defun go-mode-nesting (&optional pos)
  "Return the nesting at point POS.  The nesting is a list
of (START . END) pairs for all braces, parens, and brackets
surrounding POS, starting at the inner-most nesting.  START is
the location of the open character.  END is the location of the
close character or nil if the nesting scanner has not yet
encountered the close character."
  (let ((start (go-mode-cs)))
    (when start (goto-char (1- start))))
  (let (pairs)
    (while (backward-up-list 1 t)
      (save-excursion
	(push (cons (point) (and (goto-matched-parenthesis) (point))) pairs)))
    (nreverse pairs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;

(defvar *go-mode-non-terminating-keywords-regexp*
  (let* ((kws *go-mode-keywords*)
	 (kws (remove "break" kws))
	 (kws (remove "continue" kws))
	 (kws (remove "fallthrough" kws))
	 (kws (remove "return" kws)))
    ;(regexp-opt kws 'words))
    (format nil "~{~A~^\\|~}" (sort (mapcar #'regexp-quote kws) #'string<)))
  "Regular expression matching all Go keywords that *do not*
implicitly terminate a statement.")

(defun go-mode-semicolon-p ()
  "True iff point immediately follows either an explicit or
implicit semicolon.  Point should immediately follow the last
token on the line."

  ;; #Semicolons
  (case (char-before (point))
    ((#\;) t)
    ;; String literal
    ((#\' #\" #\`) t)
    ;; One of the operators and delimiters ++, --, ), ], or }
    ((#\+) (eq (char-before (1- (point))) #\+))
    ((#\-) (eq (char-before (1- (point))) #\-))
    ((#\) #\] #\}) t)
    ;; An identifier or one of the keywords break, continue,
    ;; fallthrough, or return or a numeric literal
    (t
     (save-excursion
       (when (skip-syntax-spec-backward "w_")
	 (not (looking-at *go-mode-non-terminating-keywords-regexp*)))))))

(defun go-mode-backward-skip-comments ()
  "Skip backward over comments and whitespace."
  ;; only proceed if point is in a comment or white space
  (while (or (let ((start (go-mode-in-comment (point))))
	       (when start (goto-char (1- start))))
	     (skip-syntax-spec-backward " /"))))

(defun go-mode-indentation ()
  "Compute the ideal indentation level of the current line.

To the first order, this is the brace depth of the current line,
plus parens that follow certain keywords.  case, default, and
labels are outdented one level, and continuation lines are
indented one level."

  (save-excursion
    (back-to-indentation)
    (multiple-value-bind (syn start) (go-mode-cs)
      ;; Treat comments and strings differently only if the beginning
      ;; of the line is contained within them

      ;; What type of context am I in?
      (case syn
	(:string
	 ;; Inside a multi-line string.  Don't mess with indentation.
	 nil)
	(:comment
	 ;; Inside a general comment
	 (goto-char start)
	 (forward-char 1)
	 (current-column))
	(t
	 ;; Not in a multi-line string or comment
	 (let ((indent 0)
	       (inside-indenting-paren nil))
	   ;; Count every enclosing brace, plus parens that follow
	   ;; import, const, var, or type and indent according to
	   ;; depth.  This simple rule does quite well, but also has a
	   ;; very large extent.  It would be better if we could mimic
	   ;; some nearby indentation.
	   (save-excursion
	     (skip-chars-forward "})")
	     (let ((first t))
	       (dolist (nest (go-mode-nesting))
		 (case (char-after (car nest))
		   (#\{
		    (incf indent *go-mode-tab-width*))
		   (#\(
		    (goto-char (car nest))
		    (go-mode-backward-skip-comments)
		    (backward-char)
		    ;; Really just want the token before
		    (when (save-excursion
			    (backward-sexp)
			    (looking-at "import\\|const\\|var\\|type\\|package"))
		      (incf indent *go-mode-tab-width*)
		      (when first
			(setq inside-indenting-paren t)))))
		 (setq first nil))))

	   ;; case, default, and labels are outdented 1 level
	   (when (looking-at "\\<case\\>\\|\\<default\\>\\|\\w+\\s *:\\(\\S.\\|$\\)")
	     (decf indent *go-mode-tab-width*))

	   (when (looking-at "\\w+\\s *:.+,\\s *$")
	     (incf indent *go-mode-tab-width*))

	   ;; Continuation lines are indented 1 level
	   (beginning-of-line)		; back up to end of previous line
	   (backward-char)
	   (go-mode-backward-skip-comments) ; back up past any comments
	   (when (case (char-before (point))
		   ((#\NUL #\{ #\:)
		    ;; At the beginning of a block or the statement
		    ;; following a label.
		    nil)
		   (#\(
		    ;; Usually a continuation line in an expression,
		    ;; unless this paren is part of a factored
		    ;; declaration.
		    (not inside-indenting-paren))
		   (#\,
		    ;; Could be inside a literal.  We're a little
		    ;; conservative here and consider any comma within
		    ;; curly braces (as opposed to parens) to be a
		    ;; literal separator.  This will fail to recognize
		    ;; line-breaks in parallel assignments as
		    ;; continuation lines.
		    (let ((depth (go-mode-nesting)))
		      (and depth
			   (not (eq (char-after (caar depth)) #\{)))))
		   (t
		    ;; We're in the middle of a block.  Did the
		    ;; previous line end with an implicit or explicit
		    ;; semicolon?
		    (not (go-mode-semicolon-p))))
	     (incf indent *go-mode-tab-width*))

	   (max indent 0)))))))

(defun go-mode-current-indentation ()
  (save-excursion
    (goto-bol)
    (skip-chars-forward " \t")
    (current-virtual-column)))

(defun go-mode-indent-line ()
  "Indent the current line according to `go-mode-indentation'."
  (interactive)
  (let ((column (go-mode-indentation)))
    (if (or (not (interactive-p))
	    (save-excursion
	      (skip-chars-backward " \t")
	      (bolp)))
	(when (integerp column)
	  (smart-indentation column))
      (if (integerp column)
	  (indent-to column)
	(insert "\t"))))
  t)

(defun go-mode-newline-and-indent (&optional (arg 1))
  (interactive "*p")
  (delete-trailing-spaces)
  (insert #\LFD arg)
  (go-mode-indent-line))


(defun go-mode-list-functions ()
  (let ((fns nil))
    (save-excursion
      (while (scan-buffer "^func *\\(\\((.*)\\)? *\\_s+ *(.*)\\).*{" :regexp t :no-dup t)
	(push (list (current-line-number) (match-string 1)) fns)))
    (nreverse fns)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go mode
;;

;;;###autoload
(defun go-mode ()
  "Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt."
  (interactive)
  
  (kill-all-local-variables)
  (setq mode-name "Go"
	buffer-mode 'go-mode)
  (use-syntax-table *go-mode-syntax-table*)
  
  ;; Font lock
  (make-local-variable 'regexp-keyword-list)
  (setq regexp-keyword-list (go-mode-regexp-keyword-list))
  (and *go-mode-keyword-file*
       (null *go-mode-keyword-hash-table*)
       (setq *go-mode-keyword-hash-table*
	     (load-keyword-file *go-mode-keyword-file* )))

  (when *go-mode-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *go-mode-keyword-hash-table*))

  ;; Indentation
  (setq mode-specific-indent-command #'go-mode-indent-line)
;  (make-local-variable 'post-buffer-modified-hook)
;  (add-hook 'post-buffer-modified-hook #'go-mode-delayed-electric-hook)

  ;; list-function
  (make-local-variable 'ed::build-summary-function)
  (setq ed::build-summary-function 'go-mode-list-functions)

  ;; Go style
  (setq indent-tabs-mode t)
  (use-keymap *go-mode-map*)
  (set-tab-columns *go-mode-tab-width* (selected-buffer))
  (run-hooks '*go-mode-hook*)
  )

;;;###autoload
(unless (find #0='("\\.go$" . go-mode) *auto-mode-alist* :test 'equal)
  (push #0# *auto-mode-alist*))


#+:nil
(defun go-mode-reload ()
  "Reload go-mode.el and put the current buffer into Go mode.
Useful for development work."

  (interactive)
  (unload-feature 'go-mode)
  (require 'go-mode)
  (go-mode))

;;;###autoload
#|
(defun gofmt ()
  "Pipe the current buffer through the external tool `gofmt`.
Replace the current buffer on success; display errors on failure."

  (interactive)
  (let ((currconf (current-window-configuration)))
    (let ((srcbuf (current-buffer)))
      (with-temp-buffer
	(let ((outbuf (current-buffer))
	      (errbuf (get-buffer-create "*Gofmt Errors*"))
	      (coding-system-for-read 'utf-8)    ;; use utf-8 with subprocesses
	      (coding-system-for-write 'utf-8))
	  (with-current-buffer errbuf (erase-buffer))
	  (with-current-buffer srcbuf
	    (save-restriction
	      (let (deactivate-mark)
		(widen)
		(if (= 0 (shell-command-on-region (point-min) (point-max) "gofmt"
						  outbuf nil errbuf))
		    ;; restore window config
		    ;; gofmt succeeded: replace the current buffer with outbuf,
		    ;; restore the mark and point, and discard errbuf.
		    (let ((old-mark (mark t)) (old-point (point)))
		      (set-window-configuration currconf)
		      (erase-buffer)
		      (insert-buffer-substring outbuf)
		      (goto-char (min old-point (point-max)))
		      (if old-mark (push-mark (min old-mark (point-max)) t))
		      (kill-buffer errbuf))

		  ;; gofmt failed: display the errors
		  (display-buffer errbuf)))))

	  ;; Collapse any window opened on outbuf if shell-command-on-region
	  ;; displayed it.
	  (delete-windows-on outbuf))))))

;;;###autoload
(defun gofmt-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook #'gofmt-before-save)"

  (interactive)
  (when (eq major-mode 'go-mode) (gofmt)))
|#

(defun go-mode-find-imports ()
  (save-excursion
    (beginning-of-buffer)
    (when (scan-buffer "^import" :regexp t :tail t)
      (skip-chars-forward " \t")
      (let ((p (point)))
	(if (looking-for "(")
	    (when (ignore-errors (goto-matched-parenthesis))
	      #0=(read-from-string (buffer-substring p (1+ (point)))))
	  (progn
	    (goto-eol)
	    (ignore-errors #0#)))))))

(defun godoc-getsymbol ()
  (save-excursion
    (skip-chars-backward #0="0-9A-Za-z_.")
    (let ((p (point)))
      (skip-chars-forward #0#)
      (when (> (point) p)
	(let* ((sym (buffer-substring p (point)))
	       (pkg (if (string-match "^\\([^.]+\\)\\." sym)
			(match-string 1)
		      sym))
	       (imp (find-if #'(lambda (im)
				 (string-match (concat "/" pkg "$") im))
			     (go-mode-find-imports))))
	  (if imp
	      (substitute-string sym pkg imp)
	    sym))))))

(defun godoc (query)
  (interactive "sgodoc: " :default0 (godoc-getsymbol))
  (let* ((bufname (format nil "*godoc - ~A*" query))
	 (buf (get-buffer-create bufname))
	 (km (make-sparse-keymap))
	 (wc (current-window-configuration))
	 (param (substitute-string query "\\." " ")))
    (erase-buffer buf)
    (define-key km #\q (lambda ()
			 (interactive)
			 (delete-buffer buf)
			 (set-window-configuration wc)))
    (make-process (format nil "~A ~A" *godoc-command* param) :output buf)
    (labels ((wait ()
	       (when (eq (process-status (buffer-process buf)) :exit)
		 (stop-timer #'wait)
		 (pop-to-buffer buf t t)
		 (make-local-variable 'keyword-hash-table)
		 (setq keyword-hash-table (load-keyword-file "go"))
		 (make-local-variable 'highlight-keyword)
		 (setq highlight-keyword t)
		 (make-local-variable 'regexp-keyword-list)
		 (setq regexp-keyword-list (ed::go-mode-regexp-keyword-list))
		 (use-keymap km)
		 (refresh-screen))))
      (start-timer 0.1 #'wait))))

(provide "go-mode")

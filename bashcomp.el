;;; bashcomp.el --- Bourne-Again Shell Completion -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014 Emílio Lopes
;; Copyright (C) 2009 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;;         Emílio Lopes <eclig@gmx.net>
;; Maintainer: Emílio Lopes <eclig@gmx.net>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; INTRODUCTION
;;
;; This file defines functions which use the underlying Bash process
;; in a `shell-mode' buffer to provide completions in the same way as
;; when working in a terminal.
;;
;; INSTALLATION
;;
;; The entry points to this library are the functions `bashcomp-completion-at-point'
;; and `bashcomp-wordbreak-completion-at-point'.  These are intended to be
;; added to `completion-at-point-functions'.
;;
;; The first one provides the "usual" completion behavior, while the
;; second one tries to perform filename completion after breaking the
;; word at point on the characters given by `bashcomp-wordbreaks'.
;; The order of these functions in `completion-at-point-functions' is
;; important, so be sure to add `bashcomp-wordbreak-completion-at-point'
;; *after* `bashcomp-completion-at-point'.
;;
;; Be careful if you also use other completion functions in
;; `completion-at-point-functions' as this is something the maintainer
;; did not test.
;;
;; You *must* also set `comint-prompt-regexp' to match your Bash
;; prompt, regardless of the value of `comint-use-prompt-regexp'.
;; This is needed by `comint-redirect', the library used for getting
;; the completions from the underlying shell process.
;;
;; USAGE
;;
;; Just hit TAB to have the word at point completed, like in a
;; terminal.
;;
;; There is no support for all completion styles provided by Emacs.
;; We provide a mix of `basic' and `partial-completion' (see
;; (info "(emacs)Completion Styles")): the completion must have the
;; same beginning as the text before point but we will perform
;; `partial-completion' for the part after the point, if you have it
;; enabled in `completion-styles'.
;;
;; Thus if you have `cat ~/th-f.el' at the prompt with the point after
;; the `th', hitting TAB will expand to `~/this-file.el' (provided that
;; file exists), but won't do any completion if point is e.g. after
;; the `-' or at the end of the line.
;;
;; As far as I know this is more than what Bash supports in a terminal,
;; which corresponds to the `emacs22' completion style.
;;
;; This library will read Bash's completion specifications the first
;; time you hit TAB and cache them in a table.  If you add more
;; completion specifications afterwards, use the command `bashcomp-reset'
;; to reread them.  Also use this command if your shell buffer seems
;; to misbehave and/or you see "Redirection" in your mode-line.
;;
;; SUPPORT FOR OTHER SHELLS
;;
;; In principle it's possible to add support for other shells
;; providing the equivalent of Bash's `complete' and `compgen'.
;; Please contact me if you are interested in doing this.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'shell)

;;; User options

(defgroup bashcomp nil
  "Using Bash's native completion engine in shell buffers."
  :group 'shell)

(defcustom bashcomp-add-suffix t
  "Non-nil if `bashcomp' should add a suffix to completed file names.
In case of exact or unambiguous completion add a slash (`/') if
the completed word is a directory name or a space otherwise."
  :type 'boolean
  :group 'bashcomp)
;;; Internal variables and constants

(defvar-local bashcomp-initialized nil
  "Non-nil if `bashcomp' was already initialized.")

(defvar-local bashcomp-rules (make-hash-table :test 'equal)
  "Mapping from command names to Bash's completion rules.")

(defconst bashcomp-wordbreaks '(?\" ?' ?@ ?> ?< ?= ?\; ?| ?& ?\( ?:)
  "List of word break characters.
This is the equivalent of COMP_WORDBREAKS: special characters
that are considered word breaks in some cases when doing
completion.  This was introduced initially to support file
completion in colon-separated values.")

(defconst bashcomp-candidates-prefix "\e\[bashcomp]:"
  "A prefix to be added by Bash's `compgen' to tag completion candidates.")

;;; Completion functions

;; Thank you, Stefan Monnier.
;; http://lists.gnu.org/archive/html/emacs-devel/2012-03/msg00265.html

;;;###autoload
(defun bashcomp-completion-at-point ()
  "Complete word at point using Bash's completion engine.
This function is meant to be added into `completion-at-point-functions'."
  (let* ((start (comint-line-beginning-position))
         (pos (point))
         (tokens (bashcomp-tokenize start pos))
         (current-token (car (last tokens)))
         (open-quote (bashcomp-token-quote current-token))
         (params (bashcomp-process-tokens tokens pos))
         (beg (if open-quote
                  (1+ (bashcomp-token-begin current-token))
                (bashcomp-token-begin current-token)))
         (end (bashcomp-token-end current-token)))
    (list beg
          end
          (bashcomp-generate-completion-table-fn open-quote params)
          :exclusive 'no
          :exit-function (lambda (string status)
                           (when (eq status 'finished)
                             (bashcomp-maybe-add-suffix string))))))

(defun bashcomp-wordbreak-completion-at-point ()
  "Try filename completion on the word at point after splitting on wordbreaks.
This function is meant to be added into `completion-at-point-functions'."
  (let* ((start (comint-line-beginning-position))
         (pos (point))
         (tokens (bashcomp-tokenize start pos))
         (current-token (car (last tokens)))
         (wordbreak-regexp (format "^%s" (mapconcat #'string bashcomp-wordbreaks "")))
         (token-after-wordbreak (save-excursion
                                  (skip-chars-backward wordbreak-regexp)
                                  (bashcomp-get-token pos)))
         (stub (bashcomp-token-string token-after-wordbreak)))
    (list (bashcomp-token-begin token-after-wordbreak)
          (save-excursion
            (skip-chars-forward wordbreak-regexp (bashcomp-token-end current-token))
            (point))
          (let (completions)
            (setq completions
                  (lazy-completion-table
                   completions
                   (lambda ()
                     (bashcomp-generate-completions (get-buffer-process (current-buffer))
                                                    (bashcomp-compgen -f -- ,stub)
                                                    stub)))))
          :exclusive 'no)))

(defun bashcomp-generate-completion-table-fn (open-quote params)
  (let (completions)
    (setq completions
          (lazy-completion-table
           completions
           (lambda ()
             (bashcomp-get-completions open-quote params))))))

(defun bashcomp-get-completions (open-quote params)
  (destructuring-bind (line point cword stub words) params
    (let ((process (get-buffer-process (current-buffer))))
      (unless bashcomp-initialized
        (bashcomp-initialize process)
        (setq bashcomp-initialized t))
      (mapcar (lambda (str)
                (bashcomp-escape-candidate str open-quote))
              (bashcomp-generate-completions
               process
               (lambda (stub)
                 (bashcomp-generate-line line point words cword stub))
               stub)))))

(defun bashcomp-maybe-add-suffix (string)
  (unless (or (null bashcomp-add-suffix)
              (memq (char-before) (append '(?/ ?\s) bashcomp-wordbreaks)))
    (let ((suffix
           (if (file-directory-p (comint-directory (shell-unquote-argument string)))
               "/"
             " ")))
      (if (looking-at suffix)
          (goto-char (match-end 0))
        (insert suffix)))))


;;; Token functions

(defsubst bashcomp-token-new (string start end)
  "Return a new token containing STRING extending from START to END."
  (list
   (cons 'str string)
   (cons 'range (cons start end))))

(defsubst bashcomp-token-range (token)
  "Return the TOKEN range as a cons: (start . end)."
  (cdr (assq 'range token)))

(defsubst bashcomp-token-begin (token)
  "Return the buffer position where TOKEN starts."
  (cadr (assq 'range token)))

(defsubst bashcomp-token-end (token)
  "Return the buffer position where TOKEN ends."
  (cddr (assq 'range token)))

(defsubst bashcomp-token-set-end (token pos)
  "Set the end position of TOKEN to the POS."
  (setcdr (bashcomp-token-range token) pos))

(defsubst bashcomp-token-append-string (token str)
  "Append to TOKEN the string STR."
  (let ((str-cons (assq 'str token)))
    (setcdr str-cons (concat (cdr str-cons) str))))

(defsubst bashcomp-token-string (token)
  "Return the TOKEN string."
  (cdr (assq 'str token)))

(defsubst bashcomp-token-quote (token)
  "Return the quote character that was still open in TOKEN."
  (cdr (assq 'quote token)))


;;; Functions: parsing and tokenizing

;; TODO: use `shell-quote-argument' instead?
;; See also `tramp-shell-quote-argument'.
(defun bashcomp-quote (word)
  "Quote WORD as appropriate for passing as an arguments to the shell."
  (if (string-match-p "^[a-zA-Z0-9_./-]*$" word)
      word
    (format "'%s'" (replace-regexp-in-string "'" "'\\''" word :literal t))))

(defun bashcomp-process-tokens (tokens pos)
  "Process a command line split into TOKENS that end at POS.
This function takes a list of tokens built by `bashcomp-tokenize'
and returns the variables Bash's `compgen' function expects as a
list with the members:
 line - the relevant command between START and POS (string)
 point - position of the cursor in line (number)
 cword - 0-based index of the word to be completed in words (number)
 stub - the portion before point of the string to be completed (string)
 words - line split into words, unescaped (list of strings)"
  (let* ((this-cmd (bashcomp-extract-current-command tokens))
         (first-token (car this-cmd))
         (last-token (car (last this-cmd)))
         (words (mapcar 'bashcomp-token-string this-cmd))
         (last-word (car (last words)))
         (start (bashcomp-token-begin first-token))
         (stub (substring last-word 0 (- pos (bashcomp-token-begin last-token))))
         (end   (+ (bashcomp-token-begin last-token) (length stub))))
    (setf (car (last words)) stub)
    (list
     (buffer-substring-no-properties start end)
     (- end start)
     (- (length words) 1)
     stub
     words)))

(defun bashcomp-extract-current-command (tokens)
  "Extract from TOKENS the tokens forming the current command.
This function takes a list of TOKENS created by
`bashcomp-tokenize' for the current buffer and select the
tokens on this list that form the current command given that the
word to be completed is the last token.
For example, given this stream of tokens:
  cd /var/tmp && ls -l
if the last token is -l, it will select:
  ls -l
if the last token is /var/tmp, it will select:
  cd /var/tmp
Return a sublist of TOKENS."
  (nreverse
   (let ((command nil)
         (state 'initial))
     (dolist (token tokens)
       (let* ((string (bashcomp-token-string token))
              (terminal-p
               (and (member string '(";" "&" "|" "&&" "||"))
                    (let ((range (bashcomp-token-range token)))
                      (= (- (cdr range) (car range))
                         (length string))))))
         (cond
          (terminal-p
           (setq state 'initial)
           (setq command nil))

          ((and (eq state 'initial)
                (null (string-match-p "=" string)))
           (setq state 'args)
           (push token command))

          ((eq state 'args)
           (push token command)))))
     (or command (last tokens)))))

(defun bashcomp-tokenize (start end)
  "Tokenize the portion of the current buffer between START and END.
This function splits a Bash command line into tokens.  It knows
about quotes, escape characters and special command separators such
as ;, | and &&.
Return a list of tokens found between START and END.  Note that
the last token might end past END."
  (save-excursion
    (goto-char start)
    (let ((tokens '()))
      (while (progn
               (skip-chars-forward " \t\n\r" end)
               (push (bashcomp-get-token) tokens)
               (< (point) end)))
      (nreverse tokens))))

(defun bashcomp-get-token (&optional limit)
  "Return the next token in the current buffer.
This function expects the point to be either at the start of a
new token or just after a closing quote in a token.
Optional argument LIMIT specifies the point at which tokenization
should stop.
Return a new token.  Note that the string in a token is never
escaped.  For example, if the token is 'hello world', the string
contains \"hello world\", without the quotes."
  (bashcomp-collect-token (bashcomp-token-new "" (point) (point)) nil limit))

(defun bashcomp-collect-token (token quote &optional limit)
  "Collect characters in TOKEN.
TOKEN is the token currently being built.
QUOTE specifies the currently active quotation character: either
nil, ?'  or ?\".
Tokenization stops either when the token ends or when the buffer
position given by optional argument LIMIT (if any) is reached.
Return TOKEN."
  ;; parse the token elements at the current position and
  ;; append them
  (let ((beg (point)))
    (when (zerop (skip-chars-forward ";&|" limit))
      (skip-chars-forward (bashcomp-nonsep quote) limit))
    (bashcomp-token-append-string
     token
     (buffer-substring-no-properties beg (point))))
  (let ((next-char (char-after)))
    (cond
     ;; an escaped char
     ((and next-char (= ?\\ next-char))
      (forward-char)
      (let ((next-char (char-after)))
        (when next-char
          (forward-char)
          (bashcomp-token-append-string token (char-to-string next-char))))
      (bashcomp-collect-token token quote limit))
     ;; opening quote
     ((and (not quote) next-char (memq next-char '(?\' ?\")))
      (forward-char)
      (bashcomp-collect-token token next-char limit))
     ;; closing quote
     ((and quote next-char (= quote next-char))
      (forward-char)
      (bashcomp-collect-token token nil limit))
     ;; space inside a quote
     ((and quote next-char (/= quote next-char) (or (null limit) (< (point) limit)))
      (forward-char)
      (bashcomp-token-append-string token (char-to-string next-char))
      (bashcomp-collect-token token quote limit))
     ;; word end or limit reached
     (t
      (when quote
        (push (cons 'quote quote) token))
      (bashcomp-token-set-end token (point))
      token))))

(defconst bashcomp-nonsep-alist
  '((nil . "^ \t\n\r;&|'\"\\\\#")
    (?'  . "^ \t\n\r'")
    (?\" . "^ \t\n\r\"\\\\"))
  "Alist of sets of non-breaking characters.
Keeps a regexp specifying the set of non-breaking characters for
all quoting environment (no quote, single quote and double
quote).  Get it using `bashcomp-nonsep'.")

(defun bashcomp-nonsep (quote)
  "Return the set of non-breaking characters when QUOTE is the current quote.
QUOTE should be nil, ?' or ?\"."
  (cdr (assq quote bashcomp-nonsep-alist)))


;;; Getting completion candidates from Bash

(defmacro bashcomp-compgen (&rest args)
  `(concat (format "compgen -P '%s' " bashcomp-candidates-prefix)
           (mapconcat (lambda (s)
                        (bashcomp-quote (format "%s" s)))
                      (backquote ,args)
                      " ")
           " 2>/dev/null"))

(defun bashcomp-generate-completions (process command stub)
  "Run compgen in process PROCESS using command COMMAND.
COMMAND can be a string or a function: a string is used as is; a
function should accept one argument `stub' and return the
completion command to be called to complete it.  This allows for
recalculating the completion command when dynamically loaded
completion rules are being used."
  (let* ((cmd (if (functionp command) (funcall command stub) command))
         (completions (bashcomp-generate-completions-1 process cmd)))
    ;; TODO: consider using catch/throw (with catch in
    ;; `bashcomp-completion-at-point' e.g.) for restarts
    (if (equal completions '("*bashcomp_restart*"))
        (progn
          ;; TODO: only reread completion rules for the corresponding program!!!
          (bashcomp-readin-completion-rules process bashcomp-rules)
          (bashcomp-generate-completions process command stub))
      completions)))

(defun bashcomp-generate-completions-1 (process command)
  (bashcomp-call-with-temp-buffer
   (lambda (temp-buffer)
     (bashcomp-send command process temp-buffer)
     (bashcomp-extract-candidates temp-buffer))))

(defun bashcomp-extract-candidates (buffer)
  "Extract the completion candidates in buffer BUFFER."
  (let ((buffer-lines
         (with-current-buffer buffer
           (save-match-data
             (split-string (buffer-string) "\n" t))))
        list)
    (dolist (line buffer-lines (nreverse list))
      (and (string-prefix-p bashcomp-candidates-prefix line)
           (push (substring line
                            (length bashcomp-candidates-prefix)
                            (string-match-p (rx (+ (char space)) eol) line))
                 list)))))

(defun bashcomp-escape-candidate (completion-candidate open-quote)
  "Escapes COMPLETION-CANDIDATE.
This function escapes all special characters in the result of
Bash completion.  It does nothing if COMPLETION-CANDIDATE looks
like a quoted string.
It uses escape characters appropriate for the quote defined in
OPEN-QUOTE, either nil, ' or \".
Return a possibly escaped version of COMPLETION-CANDIDATE."
  (cond
   ((and (null open-quote)
         (null (string-match-p "^['\"]" completion-candidate)))
    (replace-regexp-in-string "\\([ '\"#]\\)" "\\\\\\1" completion-candidate))
   ((eq ?' open-quote)
    (replace-regexp-in-string "'" "'\\''" completion-candidate :literal t))
   ((eq ?\" open-quote)
    (replace-regexp-in-string "\"" "\\\"" completion-candidate :literal t))
   (t
    completion-candidate)))


;;; Completion table

(defun bashcomp-initialize-rules (buffer rules)
  "Use the contents of BUFFER to initialize hash table RULES.
BUFFER should contain the output of \"complete -p\"."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (while (= 0 (forward-line -1))
        (bashcomp-add-rule
         (mapcar 'bashcomp-token-string
                 (bashcomp-tokenize (line-beginning-position) (line-end-position)))
         rules)))))

(defun bashcomp-add-rule (words rules)
  "Add the completion rule defined by WORDS to the hash table RULES.
The hash key is the command name for which a the rule is defined."
  (when (string= "complete" (pop words))
    (let ((command (car (last words)))
          (options (nbutlast words)))
      (when (and command options)
        (puthash command options rules)))))

(defun bashcomp-specification (command)
  "Return the completion specification for COMMAND or nil, if none found."
  (or (gethash command bashcomp-rules)
      (gethash (file-name-nondirectory command) bashcomp-rules)
      (and (memq system-type '(ms-dos windows-nt))
           (gethash
            (file-name-nondirectory (file-name-sans-extension command))
            bashcomp-rules))
      ;; "-D" is the default completion spec
      (gethash "-D" bashcomp-rules)))

(defun bashcomp-generate-line (line pos words cword stub)
  "Generate a command-line that calls Bash's `compgen'.
This function looks for a completion rule matching the command
name in LINE.  If it finds one, it uses it.  Otherwise, it tries
to complete the current word as a filename.
LINE is the command-line to complete.
POS is the position of the cursor on LINE
WORDS is the content of LINE split by words and unescaped
CWORD is the word 0-based index of the word to complete in WORDS
If the compgen argument set specifies a custom function or command, the
arguments will be passed to this function or command as:
 COMP_LINE, taken from LINE
 COMP_POINT, taken from POS
 COMP_WORDS, taken from WORDS (a Bash array)
 COMP_CWORD, taken for CWORD
Return a Bash command-line that calls compgen to get the completion
candidates."
  (let* ((command (car words))
         (compgen-args (bashcomp-specification command)))
    (cond
     ((= cword 0)
      ;; a command. let emacs expand executable, let Bash
      ;; expand builtins, aliases and functions
      (bashcomp-compgen -b -c -a -A function -- ,stub))

     ((not compgen-args)
      ;; no completion configured for this command
      (bashcomp-compgen -f -- ,stub))

     ((or (member "-F" compgen-args) (member "-C" compgen-args))
      ;; custom completion with a function or command
      (let* ((args (copy-tree compgen-args))
             (function (or (member "-F" args) (member "-C" args)))
             (function-name (car (cdr function))))
        (setcar function "-F")
        (setcar (cdr function) "__bash_complete_wrapper")
        (concat (format "__BASH_COMPLETE_WRAPPER=%s "
                        (bashcomp-quote
                         (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
                                 (bashcomp-quote line)
                                 pos
                                 cword
                                 (mapconcat 'bashcomp-quote words " ")
                                 (bashcomp-quote function-name))))
                (bashcomp-compgen ,@args -- ,stub))))
     (t
      ;; simple custom completion
      (bashcomp-compgen ,@compgen-args -- ,stub)))))

;;;###autoload
(defun bashcomp-reset ()
  "Force the next completion command to reread Bash's completion specifications."
  (interactive)
  (comint-redirect-cleanup)
  (setq bashcomp-initialized nil))

(defun bashcomp-send (cmd process output-buffer)
  "Send CMD to the Bash process PROCESS.
CMD is a Bash command, without the final newline.  The output of
CMD, if any, goes into the buffer given by OUTPUT-BUFFER."
  (let ((process-buffer (if (processp process)
                            (process-buffer process)
                          process)))
    (unwind-protect
        (with-current-buffer process-buffer
          ;; prepend a space to CMD so that Bash doesn't add it to the
          ;; history.  Requires HISTCONTROL/HISTIGNORE to be set accordingly.
          (comint-redirect-send-command-to-process (concat " " cmd) output-buffer process nil t)
          (while (null comint-redirect-completed)
            (accept-process-output nil 1)))
      ;; make sure the clean-up is done in the right buffer.
      ;; `comint-redirect-completed' is buffer-local and
      ;; `comint-redirect-cleanup' operates on the current-buffer only.
      (with-current-buffer process-buffer
        (unless comint-redirect-completed
          (comint-redirect-cleanup))))))

(defun bashcomp-initialize (process)
  "Initialize `bashcomp' in Bash process PROCESS."
  (bashcomp-initialize-complete-wrapper process)
  (clrhash bashcomp-rules)
  (bashcomp-readin-completion-rules process bashcomp-rules))

;; put this in a parameter in case an user wants to do something smart.
(defvar bashcomp-complete-wrapper
  (concat
   "function __bash_complete_wrapper { eval $__BASH_COMPLETE_WRAPPER; test $? -eq 124 && COMPREPLY=('*bashcomp_restart*');};"
   "function quote_readline { echo \"$1\"; };")
  "*Wrapper used to call Bash's `complete' to generate completions.
You know what you are doing.")

(defun bashcomp-initialize-complete-wrapper (process)
  "Initialize the completion wrapper in process PROCESS."
  (bashcomp-call-with-temp-buffer
   (lambda (temp-buffer)
     (bashcomp-send bashcomp-complete-wrapper process temp-buffer))))

(defun bashcomp-readin-completion-rules (process rules &rest commands)
  "Read in completion rules from Bash process PROCESS into RULES, a hash table.
If optional arguments COMMANDS are given, only fetch completion
rules for those given commands."
  (bashcomp-call-with-temp-buffer
   (lambda (temp-buffer)
     (bashcomp-send
      (concat "complete -p " (mapconcat #'identity commands " "))
      process
      temp-buffer)
     (bashcomp-initialize-rules temp-buffer rules))))

(defmacro bashcomp-call-with-temp-buffer (thunk)
  "Call THUNK with a freshly created temporary buffer as an argument.
Like `with-temp-buffer' but does not change the current buffer."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       (unwind-protect
           (funcall ,thunk ,temp-buffer)
         (and (buffer-name ,temp-buffer)
              (kill-buffer ,temp-buffer))))))

(provide 'bashcomp)
;;; bashcomp.el ends here

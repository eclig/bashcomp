;;; bacom.el --- Bourne-Again Completion in a shell buffer

;; Copyright (C) 2013, 2014 Emílio Lopes
;; Copyright (C) 2009 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;;         Emílio Lopes <eclig@gmx.net>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; This file define functions which use the underlying Bash process in
;; a `shell-mode' buffer to provide completion.
;;
;; Bash completion for emacs:
;;
;; - is aware of Bash builtins, aliases and functions
;; - does file expansion inside of colon-separated variables
;;   and after redirections (> or <)
;; - escapes special characters when expanding file names
;; - is configurable through programmable Bash completion
;;
;; A simpler and more complete alternative to bacom.el is to
;; run a Bash shell in a buffer in term mode(M-x `ansi-term').
;; Unfortunately, many Emacs editing features are not available when
;; running in term mode.  Also, term mode is not available in
;; shell-command prompts.
;;
;; INSTALLATION
;;
;; ...
;;
;; 4. Set `comint-prompt-regexp' to match your Bash prompt, regardless
;;    of the value of `comint-use-prompt-regexp'.  This is needed by
;;    `comint-redirect', the library used for getting the completions
;;    from the underlying shell process.
;;
;; You'll get better results if you turn on Bash's programmable completion.
;; On Debian-based systems this means running:
;;   sudo apt-get install bash-completion
;; and then adding this to your .bashrc:
;;   . /etc/bash_completion
;;
;; Right after enabling Bash's programmable completion, and whenever
;; you make changes to you .bashrc, call `bacom-reset' to make sure
;; Bash completion takes your new settings into account.
;;
;; CAVEATS
;;
;; Using the underlying Shell process for doing the completion has some
;; important disadvantages:
;; - Bash completion is slower than standard Emacs completion.
;; - the first completion can take a bit longer, since a table of
;;   available Bash completions needs to be initialized.
;; - The variable `comint-prompt-regexp' hast be set to the
;;   correct prompt for your Shell.
;;
;; bacom.el does not work on XEmacs?

(require 'cl-lib)
(require 'comint)
(require 'shell)

;;; Code:

;;; User options

(defgroup bacom nil
  "Bash configurable command-line completion "
  :group 'shell
  :group 'shell-command)

;;; Internal variables and constants

(defvar-local bacom-initialized nil
  "Non-nil if `bacom' was already initialized.")

(defvar-local bacom-rules (make-hash-table :test 'equal)
  "Mapping from command names to Bash's completion rules.")

(defconst bacom-wordbreaks '(?\" ?' ?@ ?> ?< ?= ?\; ?| ?& ?\( ?:)
  "List of word break characters.
This is the equivalent of COMP_WORDBREAKS: special characters
that are considered word breaks in some cases when doing
completion.  This was introduced initially to support file
completion in colon-separated values.")

(defconst bacom-candidates-prefix "\e\[bacom]:"
  "A prefix to be added by Bash's `compgen' to tag completion candidates.")

;;; Completion functions

;;;###autoload
(defun bacom-dynamic-complete ()
  "Complete word at cursor using Bash completion.
This function is meant to be added into
`shell-dynamic-complete-functions' or
`shell-command-complete-functions'.  It uses `comint' to figure
out what the current command is and calls
`comint-dynamic-simple-complete' to do the completion.
If a match was found, it is displayed as is usual for comint
completion.  Return nil if no match was found."
  (let* ((process (get-buffer-process (current-buffer)))
         (start (comint-line-beginning-position))
         (pos (point))
         (tokens (bacom-tokenize start pos))
         (current-token (car (last tokens)))
         (open-quote (bacom-token-quote current-token))
         (parsed (bacom-process-tokens tokens pos)))
    (unless bacom-initialized
      (bacom-initialize process)
      (setq bacom-initialized t))
    (destructuring-bind (line point cword stub words) parsed
      (let ((completions
             (bacom-generate-completions
              process
              (lambda (stub)
                (bacom-generate-line line point words cword stub))
              stub
              open-quote)))
        (if completions
            (list (if open-quote
                      (1+ (bacom-token-begin current-token))
                    (bacom-token-begin current-token))
                  (bacom-token-end current-token)
                  completions
                  :exit-function
                  (lambda (string status)
                    (when (eq status 'finished)
                      (unless (memq (char-before) (append '(?/ ?\s) bacom-wordbreaks))
                        (let ((suffix
                               (if (file-directory-p (comint-directory (shell-unquote-argument string)))
                                   "/"
                                 " ")))
                          (if (looking-at suffix)
                              (goto-char (match-end 0))
                            (insert suffix)))))))
          ;; No standard completion found, try filename completion after a wordbreak
          (bacom-dynamic-wordbreak-complete process current-token pos))))))

(defun bacom-dynamic-wordbreak-complete (process current-token pos)
  (let* ((wordbreak-regexp (format "^%s" (mapconcat #'string bacom-wordbreaks "")))
         (token-after-wordbreak (save-excursion
                                  (skip-chars-backward wordbreak-regexp)
                                  (bacom-get-token pos)))
         (stub (bacom-token-string token-after-wordbreak)))
    ;; TODO: Warning: reference to free variable `open-quote'
    (let ((completions (bacom-generate-completions process
                                                   (bacom-compgen -f -- ,stub)
                                                   stub
                                                   open-quote)))
      (when completions
        (list (bacom-token-begin token-after-wordbreak)
              (save-excursion
                (skip-chars-forward wordbreak-regexp (bacom-token-end current-token))
                (point))
              completions
              :exclusive 'no)))))



;;; Token functions

(defsubst bacom-token-new (string start end)
  "Return a new token containing STRING extending from START to END."
  (list
   (cons 'str string)
   (cons 'range (cons start end))))

(defsubst bacom-token-range (token)
  "Return the TOKEN range as a cons: (start . end)."
  (cdr (assq 'range token)))

(defsubst bacom-token-begin (token)
  "Return the buffer position where TOKEN starts."
  (cadr (assq 'range token)))

(defsubst bacom-token-end (token)
  "Return the buffer position where TOKEN ends."
  (cddr (assq 'range token)))

(defsubst bacom-token-set-end (token pos)
  "Set the end position of TOKEN to the POS."
  (setcdr (bacom-token-range token) pos))

(defsubst bacom-token-append-string (token str)
  "Append to TOKEN the string STR."
  (let ((str-cons (assq 'str token)))
    (setcdr str-cons (concat (cdr str-cons) str))))

(defsubst bacom-token-string (token)
  "Return the TOKEN string."
  (cdr (assq 'str token)))

(defsubst bacom-token-quote (token)
  "Return the quote character that was still open in TOKEN."
  (cdr (assq 'quote token)))


;;; Functions: parsing and tokenizing

;; TODO: use `shell-quote-argument' instead?
;; See also `tramp-shell-quote-argument'.
(defun bacom-quote (word)
  "Quote WORD as appropriate for passing as an arguments to the shell."
  (if (string-match-p "^[a-zA-Z0-9_./-]*$" word)
      word
    (format "'%s'" (replace-regexp-in-string "'" "'\\''" word :literal t))))

;; TODO: not used anywhere, except in the regression tests
(defun bacom-parse-line (start pos)
  (bacom-process-tokens
   (bacom-tokenize start pos) pos))

(defun bacom-process-tokens (tokens pos)
  "Process a command line split into TOKENS that end at POS.
This function takes a list of tokens built by `bacom-tokenize'
and returns the variables Bash's `compgen' function expects as a
list with the members:
 line - the relevant command between START and POS (string)
 point - position of the cursor in line (number)
 cword - 0-based index of the word to be completed in words (number)
 stub - the portion before point of the string to be completed (string)
 words - line split into words, unescaped (list of strings)
"
  (bacom-process-tokens-1 (bacom-parse-current-command tokens) pos))

(defun bacom-process-tokens-1 (tokens pos)
  (let* ((first-token (car tokens))
         (last-token (car (last tokens)))
         (start (or (bacom-token-begin first-token) pos))
         (end   (or (bacom-token-end last-token) pos))
         (words (mapcar 'bacom-token-string tokens))
         (stub  (car (last words))))
    (list
     (buffer-substring-no-properties start pos)
     (- pos start)
     (- (length words) 1)
     stub
     words)))

(defun bacom-parse-current-command (tokens)
  "Extract from TOKENS the tokens forming the current command.
This function takes a list of TOKENS created by
`bacom-tokenize' for the current buffer and select the
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
       (let* ((string (bacom-token-string token))
              (terminal-p
               (and (member string '(";" "&" "|" "&&" "||"))
                    (let ((range (bacom-token-range token)))
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

(defun bacom-tokenize (start end)
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
               (push (bacom-get-token) tokens)
               (< (point) end)))
      (nreverse tokens))))

(defun bacom-get-token (&optional limit)
  "Return the next token in the current buffer.
This function expects the point to be either at the start of a
new token or just after a closing quote in a token.
Optional argument LIMIT specifies the point at which tokenization
should stop.
Return a new token.  Note that the string in a token is never
escaped.  For example, if the token is 'hello world', the string
contains \"hello world\", without the quotes."
  (bacom-collect-token (bacom-token-new "" (point) (point)) nil limit))

(defun bacom-collect-token (token quote &optional limit)
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
      (skip-chars-forward (bacom-nonsep quote) limit))
    (bacom-token-append-string
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
          (bacom-token-append-string token (char-to-string next-char))))
      (bacom-collect-token token quote limit))
     ;; opening quote
     ((and (not quote) next-char (memq next-char '(?\' ?\")))
      (forward-char)
      (bacom-collect-token token next-char limit))
     ;; closing quote
     ((and quote next-char (= quote next-char))
      (forward-char)
      (bacom-collect-token token nil limit))
     ;; space inside a quote
     ((and quote next-char (/= quote next-char) (or (null limit) (< (point) limit)))
      (forward-char)
      (bacom-token-append-string token (char-to-string next-char))
      (bacom-collect-token token quote limit))
     ;; word end or limit reached
     (t
      (when quote
        (push (cons 'quote quote) token))
      (bacom-token-set-end token (point))
      token))))

(defconst bacom-nonsep-alist
  '((nil . "^ \t\n\r;&|'\"\\\\#")
    (?'  . "^ \t\n\r'")
    (?\" . "^ \t\n\r\"\\\\"))
  "Alist of sets of non-breaking characters.
Keeps a regexp specifying the set of non-breaking characters for
all quoting environment (no quote, single quote and double
quote).  Get it using `bacom-nonsep'.")

(defun bacom-nonsep (quote)
  "Return the set of non-breaking characters when QUOTE is the current quote.
QUOTE should be nil, ?' or ?\"."
  (cdr (assq quote bacom-nonsep-alist)))


;;; Getting completion candidates from Bash

(defmacro bacom-compgen (&rest args)
  `(concat (format "compgen -P '%s' " bacom-candidates-prefix)
           (mapconcat (lambda (s)
                        (bacom-quote (format "%s" s)))
                      (backquote ,args)
                      " ")
           " 2>/dev/null"))

(defun bacom-generate-completions (process command stub open-quote)
  "Run compgen command COMMAND in process PROCESS.
COMMAND can be a string or a function: a string is used as is; a
function should accept one argument `stub' and return the
completion command to be called to complete it.  This allows for
recalculating the completion command when dynamically loaded
completion rules are being used.
Each completion candidate is then passed to `bacom-escape-candidate',
which sees."
  (let* ((cmd (if (functionp command) (funcall command stub) command))
         (completions (bacom-generate-completions-1 process cmd)))
    ;; TODO: consider using catch/throw (with catch in
    ;; `bacom-dynamic-complete' e.g.) for restarts
    (if (equal completions '("*bacom_restart*"))
        (progn
          ;; TODO: only reread completion rules for the corresponding program!!!
          (bacom-readin-completion-rules process bacom-rules)
          (bacom-generate-completions process command stub open-quote))
      (mapcar (lambda (str)
                (bacom-escape-candidate str open-quote))
              completions))))

(defun bacom-generate-completions-1 (process command)
  (bacom-call-with-temp-buffer
   (lambda (temp-buffer)
     (bacom-send command process temp-buffer)
     (bacom-extract-candidates temp-buffer))))

(defun bacom-extract-candidates (buffer)
  "Extract the completion candidates in buffer BUFFER."
  (let ((buffer-lines
         (with-current-buffer buffer
           (save-match-data
             (split-string (buffer-string) "\n" t))))
        list)
    (dolist (line buffer-lines (nreverse list))
      (and (string-prefix-p bacom-candidates-prefix line)
           (push (substring line
                            (length bacom-candidates-prefix)
                            (string-match-p (rx (+ (char space)) eol) line))
                 list)))))

(defun bacom-escape-candidate (completion-candidate open-quote)
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

(defun bacom-initialize-rules (buffer rules)
  "Initialize hash table RULES from the contents of BUFFER.
BUFFER should contain the output of \"complete -p\"."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (while (= 0 (forward-line -1))
        (bacom-add-rule
         (mapcar 'bacom-token-string
                 (bacom-tokenize (line-beginning-position) (line-end-position)))
         rules)))))

(defun bacom-add-rule (words rules)
  "Add the completion rule defined by WORDS to the hash table RULES.
The hash key is the command name for which a the rule is defined."
  (when (string= "complete" (pop words))
    (let ((command (car (last words)))
          (options (nbutlast words)))
      (when (and command options)
        (puthash command options rules)))))

(defun bacom-specification (command)
  "Return the completion specification for COMMAND or nil, if none found."
  (or (gethash command bacom-rules)
      (gethash (file-name-nondirectory command) bacom-rules)
      (and (memq system-type '(ms-dos windows-nt))
           (gethash
            (file-name-nondirectory (file-name-sans-extension command))
            bacom-rules))
      ;; "-D" is the default completion spec
      (gethash "-D" bacom-rules)))

(defun bacom-generate-line (line pos words cword stub)
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
         (compgen-args (bacom-specification command)))
    (cond
     ((= cword 0)
      ;; a command. let emacs expand executable, let Bash
      ;; expand builtins, aliases and functions
      (bacom-compgen -b -c -a -A function -- ,stub))

     ((not compgen-args)
      ;; no completion configured for this command
      (bacom-compgen -f -- ,stub))

     ((or (member "-F" compgen-args) (member "-C" compgen-args))
      ;; custom completion with a function or command
      (let* ((args (copy-tree compgen-args))
             (function (or (member "-F" args) (member "-C" args)))
             (function-name (car (cdr function))))
        (setcar function "-F")
        (setcar (cdr function) "__bash_complete_wrapper")
        (concat (format "__BASH_COMPLETE_WRAPPER=%s "
                        (bacom-quote
                         (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
                                 (bacom-quote line)
                                 pos
                                 cword
                                 (mapconcat 'bacom-quote words " ")
                                 (bacom-quote function-name))))
                (bacom-compgen ,@args -- ,stub))))
     (t
      ;; simple custom completion
      (bacom-compgen ,@compgen-args -- ,stub)))))

;;;###autoload
(defun bacom-reset ()
  "Force the next completion command to reread the completion table.
Call this function if you have updated your ~/.bashrc or any Bash init scripts
and would like Bash completion in Emacs to take these changes into account."
  (interactive)
  (setq bacom-initialized nil))

(defun bacom-send (cmd process output-buffer)
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

(defun bacom-initialize (process)
  "Initialize `bacom' in Bash process PROCESS."
  (bacom-initialize-complete-wrapper process)
  (clrhash bacom-rules)
  (bacom-readin-completion-rules process bacom-rules))

;; put this in a parameter in case an user wants to do something smart.
(defvar bacom-complete-wrapper
  (concat
   "function __bash_complete_wrapper { eval $__BASH_COMPLETE_WRAPPER; test $? -eq 124 && COMPREPLY=('*bacom_restart*');};"
   "function quote_readline { echo \"$1\"; };")
  "*Wrapper used to call Bash's `complete' to generate completions.
You know what you are doing.")

(defun bacom-initialize-complete-wrapper (process)
  "Initialize the completion wrapper in process PROCESS."
  (bacom-call-with-temp-buffer
   (lambda (temp-buffer)
     (bacom-send bacom-complete-wrapper process temp-buffer))))

(defun bacom-readin-completion-rules (process rules &rest commands)
  "Read in completion rules from Bash process PROCESS into RULES, a hash table.
If optional arguments COMMANDS are given, only fetch completion
rules for those given commands."
  (bacom-call-with-temp-buffer
   (lambda (temp-buffer)
     (bacom-send
      (concat "complete -p " (mapconcat #'identity commands " "))
      process
      temp-buffer)
     (bacom-initialize-rules temp-buffer rules))))

(defmacro bacom-call-with-temp-buffer (thunk)
  "Call THUNK with a freshly created temporary buffer as an argument.
Like `with-temp-buffer' but does not change the current buffer."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       (unwind-protect
           (funcall ,thunk ,temp-buffer)
         (and (buffer-name ,temp-buffer)
              (kill-buffer ,temp-buffer))))))

(provide 'bacom)
;;; bacom.el ends here

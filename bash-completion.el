;;; bash-completion.el --- Bash completion for the shell buffer

;; Copyright (C) 2009 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; This file defines dynamic completion hooks for shell-mode and
;; shell-command prompts that are based on Bash completion.
;;
;; You will need shell-command.el to get tab completion in the
;; minibuffer. See http://www.namazu.org/~tsuchiya/elisp/shell-command.el
;;
;; Bash completion for emacs:
;; - is aware of Bash builtins, aliases and functions
;; - does file expansion inside of colon-separated variables
;;   and after redirections (> or <)
;; - escapes special characters when expanding file names
;; - is configurable through programmable Bash completion
;;
;; A simpler and more complete alternative to bash-completion.el is to
;; run a Bash shell in a buffer in term mode(M-x `ansi-term').
;; Unfortunately, many Emacs editing features are not available when
;; running in term mode.  Also, term mode is not available in
;; shell-command prompts.
;;
;; INSTALLATION
;;
;; 1. copy bash-completion.el into a directory that's on Emacs load-path
;; 2. add this into your .emacs file:
;;   (autoload 'bash-completion-dynamic-complete \"bash-completion\"
;;     \"Bash completion hook\")
;;   (add-hook 'shell-dynamic-complete-functions
;;      'bash-completion-dynamic-complete)
;;   (add-hook 'shell-command-complete-functions
;;      'bash-completion-dynamic-complete)
;;
;;   or simpler, but forces you to load this file at startup:
;;
;;   (require 'bash-completion)
;;   (bash-completion-setup)
;;
;; 3. reload your .emacs (M-x `eval-buffer') or restart
;;
;; 4. Set `comint-prompt-regexp' to match your Bash prompt, regardless
;;    of the value of `comint-use-prompt-regexp'.  This is needed by
;;    `comint-redirect', the library used for getting the completions
;;    from the underlying shell process.
;;
;; Once this is done, use <TAB> as usual to do dynamic completion from
;; shell mode or a shell command minibuffer, such as the one started
;; for M-x `compile'.
;;
;; You'll get better results if you turn on Bash's programmable completion.
;; On Ubuntu, this means running:
;;   sudo apt-get install bash-completion
;; and then adding this to your .bashrc:
;;   . /etc/bash_completion
;;
;; Right after enabling Bash's programmable completion, and whenever you
;; make changes to you .bashrc, call `bash-completion-reset' to make
;; sure Bash completion takes your new settings into account.
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
;; COMPATIBILITY
;;
;; bash-completion.el is quite sensitive to the OS and Bash version.
;; This package is known to work on the following environment:
;;   GNU Emacs 22.3.1 (Aquamacs 1.7)
;;   GNU Emacs 22.1.1 (OSX 10.5)
;;   GNU Emacs 22.1.1 (Ubuntu 8.04)
;;   GNU Emacs 23.0.94.1 (Ubuntu 8.10)
;;   GNU Emacs 24.1.1 (OSX 10.7)
;;   GNU Emacs 24.1.1 (OSX 10.8)
;;   GNU Emacs 24.3
;;
;; and using the following Bash versions:
;;   Bash 2.05.08
;;   Bash 3.2.17
;;   Bash 3.2.32
;;   Bash 3.2.39
;;
;; bash-completion.el does not works on XEmacs.

;;; History:
;;
;; 2013-05-27   Emilio Lopes <eclig@gmx.net>
;;
;; * Use Comint's "redirect" functionality to run completion commands.
;;
;; 2009-11-25   Stephane Zermatten <szermatt@gmail.com>
;;
;; * bash-completion-require-process: set MAILCHECK to -1
;; to disable mail check message.
;;
;; 2009-08-01   Stephane Zermatten <szermatt@gmail.com>
;;
;; * bash-completion-generate-line: add missing compgen
;; option to complete commands (duh!).
;;
;; Current version:
;; $Id$
;;

(require 'comint)

;;; Code:

;;; ---------- Customization
(defgroup bash-completion nil
  "Bash configurable command-line completion "
  :group 'shell
  :group 'shell-command)

(defcustom bash-completion-enabled t
  "Enable/Disable Bash configurable command-line completion globally.

This flag is useful for temporarily disabling Bash completion
once it's been installed.

Setting this variable to t is NOT enough to enable Bash completion.
Bash completion is only available in the environment for which
`bash-completion-dynamic-complete' has been registered. See
`bash-completion-setup' for that."
  :type '(boolean)
  :group 'bash-completion)

(defcustom bash-completion-nospace nil
  "Never let Bash add a final space at the end of a completion.

When there is only one completion candidate, Bash sometimes adds
a space at the end of the completion to move the cursor at the
appropriate position to add more command-line arguments. This
feature doesn't always work perfectly with programmable completion.

Enable this option if you find yourself having to often backtrack
to remove the extra space Bash adds after a completion."
  :type '(boolean)
  :group 'bash-completion)

;;; ---------- Internal variables and constants

(defvar-local bash-completion-initialized nil
  "Non-nil if `bash-completion-alist' was already initialized.")

(defvar-local bash-completion-alist nil
  "Maps from command name to the 'complete' arguments.

For example if the following completion is defined in Bash:
  complete -F _cdargs_aliases cdb
the following entry is added to `bash-completion-alist':
 (\"cdb\" . (\"-F\" \"_cdargs\"))

See `bash-completion-build-alist'.")

(defconst bash-completion-wordbreaks '(?\" ?' ?@ ?> ?< ?= ?\; ?| ?& ?\( ?:)
  "List of word break characters.
This is the equivalent of COMP_WORDBREAKS: special characters
that are considered word breaks in some cases when doing
completion.  This was introduced initially to support file
completion in colon-separated values.")

(defconst bash-completion-output-buffer " *bash-completion*"
  "Buffer containing output of Bash's completion functions.")

(defconst bash-completion-candidates-prefix "\e\[bash-completion]:"
  "A prefix to be added by Bash's `compgen' to tag completion candidates.")

;;; ---------- Inline functions

(defsubst bash-completion-token-new (string start end)
  "Return a new token containing STRING extending from START to END."
  (list
   (cons 'str string)
   (cons 'range (cons start end))))

(defsubst bash-completion-token-range (token)
  "Return the TOKEN range as a cons: (start . end)."
  (cdr (assq 'range token)))

(defsubst bash-completion-token-begin (token)
  "Return the buffer position where TOKEN starts."
  (cadr (assq 'range token)))

(defsubst bash-completion-token-end (token)
  "Return the buffer position where TOKEN ends."
  (cddr (assq 'range token)))

(defsubst bash-completion-token-set-end (token pos)
  "Set the end position of TOKEN to the POS."
  (setcdr (bash-completion-token-range token) pos))

(defsubst bash-completion-token-append-string (token str)
  "Append to TOKEN the string STR."
  (let ((str-cons (assq 'str token)))
    (setcdr str-cons (concat (cdr str-cons) str))))

(defsubst bash-completion-token-string (token)
  "Return the TOKEN string."
  (cdr (assq 'str token)))

(defsubst bash-completion-token-quote (token)
  "Return the quote character that was still open in TOKEN."
  (cdr (assq 'quote token)))

;;; ---------- Functions: completion

;;;###autoload
(defun bash-completion-setup ()
  "Register Bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions' and to the
completion function list of shell-command, `shell-command-complete-functions'.

This function is convenient, but it might not be the best way of enabling
Bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

  (autoload 'bash-completion-dynamic-complete \"bash-completion\"
    \"Bash completion hook\")
  (add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete))
"
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
            'bash-completion-dynamic-complete))

;;;###autoload
(defun bash-completion-dynamic-complete ()
  "Complete word at cursor using Bash completion.

This function is meant to be added into
`shell-dynamic-complete-functions' or
`shell-command-complete-functions'.  It uses `comint' to figure
out what the current command is and calls
`comint-dynamic-simple-complete' to do the completion.

If a match was found, it is displayed as is usual for comint
completion.  Return nil if no match was found."
  (when bash-completion-enabled
    (let* ((start (comint-line-beginning-position))
           (pos (point))
           (tokens (bash-completion-tokenize start pos))
           (current-token (car (last tokens)))
           (open-quote (bash-completion-token-quote current-token))
           (parsed (bash-completion-process-tokens tokens pos))
           (line  (cdr (assq 'line parsed)))
           (point (cdr (assq 'point parsed)))
           (cword (cdr (assq 'cword parsed)))
           (words (cdr (assq 'words parsed)))
           (stub  (cdr (assq 'stub parsed)))
           (completions (bash-completion-comm line point words cword stub open-quote))
           ;; Override configuration for comint-dynamic-simple-complete.
           ;; Bash adds a space suffix automatically.
           (comint-completion-addsuffix nil))
      (if completions
          (completion-in-region (if open-quote
                                    (1+ (bash-completion-token-begin current-token))
                                  (bash-completion-token-begin current-token))
                                (bash-completion-token-end current-token)
                                completions)
	;; No standard completion found, try file completion after a wordbreak
	(bash-completion-dynamic-wordbreak-complete current-token pos)))))

(defun bash-completion-dynamic-wordbreak-complete (current-token pos)
  (let* ((wordbreak-regexp (format "^%s" (mapconcat #'string bash-completion-wordbreaks "")))
         (token-after-wordbreak (save-excursion
                                  (skip-chars-backward wordbreak-regexp)
                                  (bash-completion-get-token pos)))
         (stub (bash-completion-token-string token-after-wordbreak)))
    (bash-completion-send (bash-completion-compgen -f -- ,stub))
    (let ((completions (bash-completion-extract-candidates stub open-quote)))
      (when completions
        (completion-in-region (bash-completion-token-begin token-after-wordbreak)
                              (save-excursion
                                (skip-chars-forward wordbreak-regexp (bash-completion-token-end current-token))
                                (point))
                              completions)))))

;;; ---------- Functions: parsing and tokenizing

(defun bash-completion-join (words)
  "Join WORDS into a shell command line.

All words that contain even mildly suspicious characters are
quoted using single quotes to avoid the shell interpreting them
when it shouldn't.

Return one string containing WORDS."
  (if words
      (mapconcat 'bash-completion-quote words " ")
    ""))

;; TODO: use `shell-quote-argument' instead?
;; See also `tramp-shell-quote-argument'.
(defun bash-completion-quote (word)
  "Put single quotes around WORD unless it's clearly unnecessary.

If WORD contains characters that aren't known to be harmless, this
functions adds single quotes around it and return the result."
  (if (string-match-p "^[a-zA-Z0-9_./-]*$" word)
      word
    (concat "'"
            (replace-regexp-in-string "'" "'\\''" word :literal t)
            "'")))

;; TODO: not used anywhere, except in the regression tests
(defun bash-completion-parse-line (start pos)
  "Split a command line in the current buffer between START and POS.

This function combines `bash-completion-tokenize' and
`bash-completion-process-tokens'.  It takes the same arguments as
`bash-completion-tokenize' and returns the same value as
`bash-completion-process-tokens'."
  (bash-completion-process-tokens
   (bash-completion-tokenize start pos) pos))

(defun bash-completion-process-tokens (tokens pos)
  "Process a command line split into TOKENS that end at POS.

This function takes a list of tokens built by
`bash-completion-tokenize' and returns the variables Bash's
`compgen' function expects in an association list.

Return an association list with the following symbols as keys:
 line - the relevant command between START and POS (string)
 point - position of the cursor in line (number)
 words - line split into words, unescaped (list of strings)
 stub - the portion before point of the string to be completed (string)
 cword - 0-based index of the word to be completed in words (number)"
  (bash-completion-parse-line-postprocess
   (bash-completion-parse-current-command tokens) pos))

(defun bash-completion-parse-line-postprocess (tokens pos)
  "Extract from TOKENS the data needed by compgen functions.

This function takes a list of TOKENS created by `bash-completion-tokenize'
for the current buffer and generate the data needed by compgen functions
as returned by `bash-completion-parse-line' given the cursor position POS."
  (let* ((first-token (car tokens))
         (last-token (car (last tokens)))
         (start (or (bash-completion-token-begin first-token) pos))
         (end   (or (bash-completion-token-end last-token) pos))
         (words (bash-completion-strings-from-tokens tokens))
         (stub  (cond 
                 ((and (/= start end) (= pos end))
                  (car (last words)))
                 ((and last-token (< pos end))
                  (save-excursion
                    (goto-char (bash-completion-token-begin last-token))
                    (bash-completion-token-string (bash-completion-get-token pos))))
                 (t ""))))
    (when (or (> pos end) (= start end))
      (setq words (append words '(""))))
    (list
     (cons 'line (buffer-substring-no-properties start pos))
     (cons 'point (- pos start))
     (cons 'cword (- (length words) 1))
     (cons 'stub  stub)
     (cons 'words words))))

(defun bash-completion-parse-current-command (tokens)
  "Extract from TOKENS the tokens forming the current command.

This function takes a list of TOKENS created by
`bash-completion-tokenize' for the current buffer and select the
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
       (let* ((string (bash-completion-token-string token))
              (terminal-p
               (and (member string '(";" "&" "|" "&&" "||"))
                    (let ((range (bash-completion-token-range token)))
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

(defun bash-completion-strings-from-tokens (tokens)
  "Extract the strings from TOKENS.

This function takes all strings from TOKENS and retrun it as a
list of strings.

TOKENS should be in the format returned by `bash-completion-tokenize'."
  (mapcar 'bash-completion-token-string tokens))

(defun bash-completion-tokenize (start end)
  "Tokenize the portion of the current buffer between START and END.

This function splits a Bash command line into tokens.  It knows
about quotes, escape characters and special command separators such
as ;, | and &&.

Return a list of tokens found between START and END.  Note that
the last token might end past END."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\n\r" end)
    (let ((tokens '()))
      (while (< (point) end)
        (push (bash-completion-get-token) tokens))
      (nreverse tokens))))

(defun bash-completion-get-token (&optional limit)
  "Return the next token in the current buffer.

This function expects the point to be either at the start of a
new token or just after a closing quote in a token.

Optional argument LIMIT specifies the point at which tokenization
should stop.

Return a new token.  Note that the string in a token is never
escaped.  For example, if the token is 'hello world', the string
contains \"hello world\", without the quotes."
  (bash-completion-collect-token (bash-completion-token-new "" (point) nil) nil limit))

(defun bash-completion-collect-token (token quote &optional limit)
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
      (skip-chars-forward (bash-completion-nonsep quote) limit))
    (bash-completion-token-append-string
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
          (bash-completion-token-append-string token (char-to-string next-char))))
      (bash-completion-collect-token token quote limit))
     ;; opening quote
     ((and (not quote) next-char (memq next-char '(?\' ?\")))
      (forward-char)
      (bash-completion-collect-token token next-char limit))
     ;; closing quote
     ((and quote next-char (= quote next-char))
      (forward-char)
      (bash-completion-collect-token token nil limit))
     ;; space inside a quote
     ((and quote next-char (/= quote next-char) (or (null limit) (< (point) limit)))
      (forward-char)
      (bash-completion-token-append-string token (char-to-string next-char))
      (bash-completion-collect-token token quote limit))
     ;; word end or limit reached
     (t
      (when quote
        (push (cons 'quote quote) token))
      (bash-completion-token-set-end token (point))
      (skip-chars-forward " \t\n\r" limit)
      token))))

(defconst bash-completion-nonsep-alist
  '((nil . "^ \t\n\r;&|'\"\\\\#")
    (?'  . "^ \t\n\r'")
    (?\" . "^ \t\n\r\"\\\\"))
  "Alist of sets of non-breaking characters.
Keeps a regexp specifying the set of non-breaking characters for
all quoting environment (no quote, single quote and double
quote).  Get it using `bash-completion-nonsep'.")

(defun bash-completion-nonsep (quote)
  "Return the set of non-breaking characters when QUOTE is the current quote.

QUOTE should be nil, ?' or ?\"."
  (cdr (assq quote bash-completion-nonsep-alist)))

;;; ---------- Functions: getting candidates from Bash

(defmacro bash-completion-compgen (&rest args)
  `(concat (format "compgen -P '%s' " bash-completion-candidates-prefix)
           (mapconcat (lambda (s)
                        (bash-completion-quote (format "%s" s)))
                      (backquote ,args)
                      " ")))

(defun bash-completion-comm (line pos words cword stub open-quote)
  "Setup the completion environment and call compgen, returning the result.

OPEN-QUOTE should be the quote, a character, that's still open in
the last word or nil.

The result is a list of candidates, which might be empty."
  (bash-completion-send
   (concat
    (bash-completion-generate-line line pos words cword stub)
    " 2>/dev/null"))
  (bash-completion-extract-candidates stub open-quote))

(defun bash-completion-extract-candidates (stub open-quote)
  "Extract the completion candidates for STUB.
This command takes the contents of `bash-completion-output-buffer', splits
it on newlines, post-processes the candidates and returns them as a list
of strings.  If STUB is quoted, the quote character, ' or \", should be passed
in OPEN-QUOTE.

The completion candidates are subject to post-processing by `bash-completion-postprocess',
which see."
  (bash-completion-filter-map
   (lambda (str)
     (and (bash-completion-starts-with str bash-completion-candidates-prefix)
          (bash-completion-postprocess (substring str (length bash-completion-candidates-prefix)) stub open-quote)))
   (with-current-buffer bash-completion-output-buffer
     (save-match-data
       (split-string (buffer-string) "\n" t)))))

(defun bash-completion-postprocess (str prefix &optional open-quote)
  "Post-process the completion candidate given in STR.
PREFIX is the current string being completed.  Optional argument
OPEN-QUOTE is the quote that's still open in prefix, a
character (' or \"), or nil.  Return the modified version of the
completion candidate.

Post-processing includes escaping special characters, adding a \"/\"
to directory names, merging STUB with the result.

It should be invoked with the comint buffer as the current buffer
for directory name detection to work."
  (let ((suffix ""))
    (bash-completion-addsuffix
     (let* ((rebuilt)
            (rest (cond
                   ((bash-completion-starts-with str prefix)
                    (substring str (length prefix)))
                   ;; Bash expands the home directory automatically. This is confusing
                   ;; for comint-dynamic-simple-complete
                   ((and (bash-completion-starts-with prefix "~")
                         (bash-completion-starts-with str (expand-file-name "~")))
                    (substring (concat "~/" (substring str (length (file-name-as-directory (expand-file-name "~")))))
                               (length prefix)))
                   ((bash-completion-starts-with prefix str)
                    ;; completion is a substring of prefix something's
                    ;; gone wrong. Treat it as one (useless)
                    ;; candidate.
                    (setq prefix "")
                    str)
                   ;; completion sometimes only applies to the last word, as
                   ;; defined by COMP_WORDBREAKS. This detects and works around
                   ;; this feature.
                   ((bash-completion-starts-with
                     (setq rebuilt (concat (bash-completion-before-last-wordbreak prefix) str))
                     prefix)
                    (substring rebuilt (length prefix)))
                   ;; there is no meaningful link between the prefix and
                   ;; the string. just append the string to the prefix and
                   ;; hope for the best.
                   (t str))))
       (when (bash-completion-ends-with rest " ")
         (setq rest (substring rest 0 -1))
         (unless bash-completion-nospace
           (setq suffix " ")))
       (concat prefix (bash-completion-escape-candidate rest open-quote) suffix)))))

(defmacro bash-completion-filter-map (f list)
  "Apply F to each element of LIST, returning a list of the non-nil results."
  (let ((result (make-symbol "result")))
    `(let ((,result '()))
       (dolist (x ,list ,result)
         (let ((fx (funcall ,f x)))
           (when fx
             (setq ,result (cons fx ,result)))))
       (nreverse ,result))))

(defun bash-completion-escape-candidate (completion-candidate open-quote)
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

(defconst bash-completion-known-suffixes-regexp
  (concat (regexp-opt-charset (append '(?/ ?\s) bash-completion-wordbreaks)) "$")
  "Regexp matching known suffixes for `bash-completion-addsuffix'.")

(defun bash-completion-addsuffix (str)
  "Add a directory suffix to STR if it looks like a directory.

This function looks for a directory called STR relative to the
buffer-local variable default-directory. If it exists, it returns
\(concat STR \"/\"). Otherwise it retruns STR."
  (if (and (null (string-match-p bash-completion-known-suffixes-regexp str))
           (file-accessible-directory-p (expand-file-name str default-directory)))
      (concat str "/")
    str))

(defun bash-completion-before-last-wordbreak (str)
  "Return the part of STR that comes after the last wordbreak character.
The return value does not include the worbreak character itself.

If no wordbreak was found, it returns STR.

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (car (bash-completion-last-wordbreak-split str)))

(defun bash-completion-after-last-wordbreak (str)
  "Return the part of STR that comes before the last wordbreak character.
The return value includes the worbreak character itself.

If no wordbreak was found, it returns \"\".

Wordbreaks characters are defined in 'bash-completion-wordbreak'."
  (cdr (bash-completion-last-wordbreak-split str)))

(defun bash-completion-last-wordbreak-split (str)
  "Split STR at the last wordbreak character.

The part before the last wordbreak character includes the
wordbreak character itself.  It is \"\" if no wordbreak character
was found.

The part after the last wordbreak character does not include the
wordbreak character.  It is STR if no wordbreak character was
found.

Wordbreaks characters are defined in 'bash-completion-wordbreak'.

Return a CONS containing (before . after)."
  (catch 'bash-completion-return
    (let ((end (- (length str) 1)))
      (while (>= end 0)
        (when (memq (aref str end) bash-completion-wordbreaks)
          (throw 'bash-completion-return (cons (substring str 0 (1+ end)) (substring str (1+ end)))))
        (setq end (1- end))))
    (cons "" str)))

(defun bash-completion-ends-with (str suffix)
  "Return t if STR ends with SUFFIX."
  (let ((suffix-len (length suffix))
        (str-len (length str)))
    (or
     (= 0 suffix-len)
     (and
      (>= str-len suffix-len)
      (equal (substring str (- suffix-len)) suffix)))))

;; TODO: Emacs has `string-prefix-p' since 23.2, maybe use it.
(defun bash-completion-starts-with (str prefix)
  "Return t if STR starts with PREFIX."
  (let ((prefix-len (length prefix))
        (str-len (length str)))
    (and
     (>= str-len prefix-len)
     (equal (substring str 0 prefix-len) prefix))))

;;; ---------- Functions: Bash subprocess

;; TODO: use a hash table
(defun bash-completion-build-alist (buffer)
  "Build `bash-completion-alist' from the contents of BUFFER.

BUFFER should contain the output of \"complete -p\".

Return `bash-completion-alist', which is a slightly parsed version
of the output of \"complete -p\"."
  (with-current-buffer buffer
    (save-excursion
      (setq bash-completion-alist nil)
      (goto-char (point-max))
      (while (= 0 (forward-line -1))
        (bash-completion-add-to-alist
         (bash-completion-strings-from-tokens
          (bash-completion-tokenize
           (line-beginning-position)
           (line-end-position)))))))
  bash-completion-alist)

(defun bash-completion-add-to-alist (words)
  "Add WORDS, a list of tokens from a single `complete' command, to `bash-completion-alist'.
The key of the association cons cell is the command name for
which a completion is defined by WORDS."
  (when (string= "complete" (pop words))
    (let ((command (last words))
          (options (nbutlast words)))
      (when (and command options)
        (push (append command options) bash-completion-alist))))
  bash-completion-alist)

(defun bash-completion-specification (command)
  "Return the completion specification for COMMAND.
If there is no completion specification for COMMAND in
`bash-completion-alist', return nil."
  (unless bash-completion-initialized
    (bash-completion-initialize)
    (setq bash-completion-initialized t))
  (cdr (assoc command bash-completion-alist)))

(defun bash-completion-generate-line (line pos words cword stub)
  "Generate a command-line that calls Bash's `compgen'.

This function looks into `bash-completion-alist' for a matching completion 
specification.  If it finds one, it uses it. Otherwise, it tries to
complete the current word as a filename.

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
  (let* ((command-name (file-name-nondirectory (car words)))
         (compgen-args (or (bash-completion-specification command-name)
                           ;; "-D" is the default completion spec
                           (bash-completion-specification "-D"))))
    (cond
     ((= cword 0)
      ;; a command. let emacs expand executable, let Bash
      ;; expand builtins, aliases and functions
      (bash-completion-compgen -S " " -b -c -a -A function -- ,stub))

     ((not compgen-args)
      ;; no completion configured for this command
      (bash-completion-compgen -f -- ,stub))

     ((or (member "-F" compgen-args) (member "-C" compgen-args))
      ;; custom completion with a function of command
      (let* ((args (copy-tree compgen-args))
             (function (or (member "-F" args) (member "-C" args)))
             (function-name (car (cdr function))))
        (setcar function "-F")
        (setcar (cdr function) "__bash_complete_wrapper")
        (concat (format "__BASH_COMPLETE_WRAPPER=%s "
                        (bash-completion-quote
                         (format "COMP_LINE=%s; COMP_POINT=%s; COMP_CWORD=%s; COMP_WORDS=( %s ); %s \"${COMP_WORDS[@]}\""
                                 (bash-completion-quote line)
                                 pos
                                 cword
                                 (bash-completion-join words)
                                 (bash-completion-quote function-name))))
                (bash-completion-compgen ,@args -- ,stub))))
     (t
      ;; simple custom completion
      (bash-completion-compgen ,@compgen-args -- ,stub)))))

;;;###autoload
(defun bash-completion-reset ()
  "Force the next completion command to reread the completion table.

Call this function if you have updated your ~/.bashrc or any Bash init scripts
and would like Bash completion in Emacs to take these changes into account."
  (interactive)
  (setq bash-completion-initialized nil)
  (setq bash-completion-alist nil))

(defun bash-completion-send (commandline &optional process)
  "Send COMMANDLINE to the Bash process.

COMMANDLINE should be a Bash command, without the final newline.

Optional Argument PROCESS defaults to the process associated with
the current buffer.

Once this command has run without errors, you will find the result
of the command in the buffer  `bash-completion-output-buffer'."
  (let ((process (or process (get-buffer-process (current-buffer)))))
    (with-current-buffer (get-buffer-create bash-completion-output-buffer)
      (erase-buffer))
    (let ((process-buffer (process-buffer process)))
      (unwind-protect
          (with-current-buffer process-buffer
            ;; prepend a space to COMMANDLINE so that Bash doesn't add it to the
            ;; history.  Requires HISTCONTROL/HISTIGNORE to be set accordingly.
            (comint-redirect-send-command-to-process (concat " " commandline) bash-completion-output-buffer process nil t)
            (while (null comint-redirect-completed)
              (accept-process-output nil 1)))
        ;; make sure the clean-up is done in the right buffer.
        ;; `comint-redirect-completed' is buffer-local and
        ;; `comint-redirect-cleanup' operates on the current-buffer only.
        (with-current-buffer process-buffer
          (unless comint-redirect-completed
            (comint-redirect-cleanup)))))))

(defun bash-completion-initialize ()
  "Initialize `bash-completion-alist' from the ouput of \"complete -p\"."
  (bash-completion-send
   (concat
    "function __bash_complete_wrapper { eval $__BASH_COMPLETE_WRAPPER; };"
    "function quote_readline { echo \"$1\"; };"
    "complete -p"))
  (bash-completion-build-alist bash-completion-output-buffer))

(provide 'bash-completion)
;;; bash-completion.el ends here

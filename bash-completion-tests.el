;;; bash-completion-tests.el --- Tests cases for bash-completion.el

;; Copyright (C) 2013 Emílio Lopes
;; Copyright (C) 2009 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;;         Emílio Lopes <eclig@gmx.net>
;; Keywords: processes

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
;; This file defines regression tests for the "bash-completion" package.
;; Eval these forms to run the tests:
;;
;;     (ert '(tag bash-completion))
;;     (ert '(tag bash-completion-integration))
;;
;; See Info(ert) for more information.

;;; Code:

(require 'ert)
(require 'bash-completion)
(require 'sz-testutils)

(eval-when-compile
  '(require cl-macs))

(ert-deftest bash-completion-test-join-simple ()
  :tags '(bash-completion)
  "bash-completion-join simple"
  (should (string=
           (bash-completion-join
            '("a" "hello" "world" "b" "c"))
           "a hello world b c")))

(ert-deftest bash-completion-test-join-escape-quote ()
  :tags '(bash-completion)
  "bash-completion-join escape quote"
  (should (string=
           (bash-completion-join '("a" "hel'lo" "world" "b" "c"))
           "a 'hel'\\''lo' world b c")))

(ert-deftest bash-completion-test-join-escape-space ()
  :tags '(bash-completion)
  "bash-completion-join escape space"
  (should (string=
           (bash-completion-join '("a" "hello world" "b" "c"))
           "a 'hello world' b c")))

(ert-deftest bash-completion-test-tokenize-simple ()
  :tags '(bash-completion)
  "bash-completion-tokenize simple"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello world b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello" "world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-simple-extra-spaces ()
  :tags '(bash-completion)
  "bash-completion-tokenize simple extra spaces"
  (should (equal
           (sz-testutils-with-buffer
            '("  a  hello \n world \t b \r c  ")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position 2))))
           '("a" "hello" "world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-escaped-char ()
  :tags '(bash-completion)
  "bash-completion-tokenize escaped char"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello\\-world b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello-world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-escaped-space ()
  :tags '(bash-completion)
  "bash-completion-tokenize escaped space"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello\\ world b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-escaped-hash ()
  :tags '(bash-completion)
  "bash-completion-tokenize escaped #"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello \\#world\\# b")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello" "#world#" "b"))))

(ert-deftest bash-completion-test-tokenize-double-quotes ()
  :tags '(bash-completion)
  "bash-completion-tokenize double quotes"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"hello world\" b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-double-quotes-escaped ()
  :tags '(bash-completion)
  "bash-completion-tokenize double quotes escaped"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"-\\\"hello world\\\"-\" b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "-\"hello world\"-" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-single-quotes ()
  :tags '(bash-completion)
  "bash-completion-tokenize single quotes"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"hello world\" b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bash-completion-test-tokenize-single-quotes-escaped ()
  :tags '(bash-completion)
  "bash-completion-tokenize single quotes escaped"
  (should (equal
           (sz-testutils-with-buffer
            '("a '-\\'hello world\\'-' b c")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "-\\hello" "world'- b c"))))

(ert-deftest bash-completion-test-get-token-open-single-quote ()
  :tags '(bash-completion)
  "bash-completion-tokenize with a single quote open"
  (should (string=
           (sz-testutils-with-buffer
            '("hello 'world")
            ;; 123456789
            (goto-char 7)
            (bash-completion-token-string
             (bash-completion-get-token (line-end-position))))
           "world")))

(ert-deftest bash-completion-test-tokenize-open-single-quote-limited ()
  :tags '(bash-completion)
  "bash-completion-tokenize with a single quote open limited"
  (should (string=
           (sz-testutils-with-buffer
            '("hello 'world")
            ;; 123456789
            (goto-char 7)
            (bash-completion-token-string
             (bash-completion-get-token 10)))
           "wo")))

(ert-deftest bash-completion-test-tokenize-complex-quote-mix ()
  :tags '(bash-completion)
  "bash-completion-tokenize complex quote mix"
  (should (equal
           (sz-testutils-with-buffer
            '("a hel\"lo w\"o'rld b'c d")
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("a" "hello world bc" "d"))))

(ert-deftest bash-completion-test-tokenize-unescaped-semicolon ()
  :tags '(bash-completion)
  "bash-completion-tokenize unescaped semicolon"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity;and\\ beyond"
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("to" "infinity" ";" "and beyond"))))

(ert-deftest bash-completion-test-tokenize-unescaped-and ()
  :tags '(bash-completion)
  "bash-completion-tokenize unescaped &&"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity&&and\\ beyond"
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("to" "infinity" "&&" "and beyond"))))

(ert-deftest bash-completion-test-tokenize-unescaped-or ()
  :tags '(bash-completion)
  "bash-completion-tokenize unescaped ||"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity||and\\ beyond"
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("to" "infinity" "||" "and beyond"))))

(ert-deftest bash-completion-test-tokenize-quoted-separators ()
  :tags '(bash-completion)
  "bash-completion-tokenize quoted ;&|"
  (should (equal
           (sz-testutils-with-buffer
            "to \"infinity;&|and\" beyond"
            (bash-completion-strings-from-tokens
             (bash-completion-tokenize 1 (line-end-position))))
           '("to" "infinity;&|and" "beyond"))))

(ert-deftest bash-completion-test-parse-line-cursor-at-end-of-word ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor at end of word"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "a hello world")
             (point . 13)
             (cword . 2)
             (stub . "world")
             (words "a" "hello" "world")))))

(ert-deftest bash-completion-test-parse-line-cursor-in-the-middle-of-a-word ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor in the middle of a word"
  (should (equal
           (sz-testutils-with-buffer
            "a hello wo"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "a hello wo")
             (point . 10)
             (cword . 2)
             (stub . "wo")
             (words "a" "hello" "wo")))))

(ert-deftest bash-completion-test-parse-line-cursor-at-the-beginning ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor at the beginning"
  (should (equal
           (sz-testutils-with-buffer
            " "
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "")
             (point . 0)
             (cword . 0)
             (stub . "")
             (words "")))))

(ert-deftest bash-completion-test-parse-line-cursor-in-the-middle ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor in the middle"
  (should (equal
           (sz-testutils-with-buffer
            "a hello "
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "a hello ")
             (point . 8)
             (cword . 2)
             (stub . "")
             (words "a" "hello" "")))))

(ert-deftest bash-completion-test-parse-line-cursor-at-end ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor at end"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world b c"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "a hello world b c")
             (point . 17)
             (cword . 4)
             (stub . "c")
             (words "a" "hello" "world" "b" "c")))))

(ert-deftest bash-completion-test-parse-line-complex-multi-command-line ()
  :tags '(bash-completion)
  "bash-completion-parse-line complex multi-command line"
  (should (equal
           (sz-testutils-with-buffer
            "cd /var/tmp ; ZORG=t make -"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "make -")
             (point . 6)
             (cword . 1)
             (stub . "-")
             (words "make" "-")))))

(ert-deftest bash-completion-test-parse-line-pipe ()
  :tags '(bash-completion)
  "bash-completion-parse-line pipe"
  (should (equal
           (sz-testutils-with-buffer
            "ls /var/tmp | sort -"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "sort -")
             (point . 6)
             (cword . 1)
             (stub . "-")
             (words "sort" "-")))))

(ert-deftest bash-completion-test-parse-line-escaped-semicolon ()
  :tags '(bash-completion)
  "bash-completion-parse-line escaped semicolon"
  (should (equal
           (sz-testutils-with-buffer
            "find -name '*.txt' -exec echo {} ';' -"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "find -name '*.txt' -exec echo {} ';' -")
             (point . 38)
             (cword . 7)
             (stub . "-")
             (words "find" "-name" "*.txt" "-exec" "echo" "{}" ";" "-")))))

(ert-deftest bash-completion-test-parse-line-at-var-assignment ()
  :tags '(bash-completion)
  "bash-completion-parse-line at var assignment"
  (should (equal
           (sz-testutils-with-buffer
            "cd /var/tmp ; A=f ZORG=t"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "ZORG=t")
             (point . 6)
             (cword . 0)
             (stub . "ZORG=t")
             (words "ZORG=t")))))

(ert-deftest bash-completion-test-parse-line-cursor-after-end ()
  :tags '(bash-completion)
  "bash-completion-parse-line cursor after end"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world b c "
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "a hello world b c ")
             (point . 18)
             (cword . 5)
             (stub . "")
             (words "a" "hello" "world" "b" "c" "")))))

(ert-deftest bash-completion-test-parse-line-with-escaped-quote ()
  :tags '(bash-completion)
  "bash-completion-parse-line with escaped quote"
  (should (equal
           (sz-testutils-with-buffer
            "cd /vcr/shows/Dexter\\'s"
            (bash-completion-parse-line 1 (line-end-position)))
           '((line . "cd /vcr/shows/Dexter\\'s")
             (point . 23)
             (cword . 1)
             (stub . "/vcr/shows/Dexter's")
             (words "cd" "/vcr/shows/Dexter's")))))

(ert-deftest bash-completion-test-add-rule-garbage ()
  :tags '(bash-completion)
  "bash-completion-add-rule garbage"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bash-completion-add-rule (list "just" "some" "garbage") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bash-completion-test-add-rule-empty ()
  :tags '(bash-completion)
  "bash-completion-add-rule empty"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bash-completion-add-rule nil rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bash-completion-test-add-rule-empty-string ()
  :tags '(bash-completion)
  "bash-completion-add-rule empty string"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bash-completion-add-rule (list "") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bash-completion-test-add-rule-empty-complete ()
  :tags '(bash-completion)
  "bash-completion-add-rule empty complete"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bash-completion-add-rule (list "complete") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bash-completion-test-add-rule-one-command ()
  :tags '(bash-completion)
  "bash-completion-add-rule one command"
  (should (equal
           (let ((rules (make-hash-table :test 'equal)))
             (bash-completion-add-rule (list "complete" "-e" "-F" "_cdargs_aliases" "cdb") rules)
             (gethash "cdb" rules))
           '("-e" "-F" "_cdargs_aliases"))))

(ert-deftest bash-completion-test-initialize-rules ()
  :tags '(bash-completion)
  "bash-completion-initialize-rules"
  (should (equal
           (sz-testutils-with-buffer
            (concat "\n"
                    "complete -F _cdargs_aliases cdb\n"
                    "complete -F complete_projects project\n"
                    "complete -F complete_projects pro\n"
                    "complete -F _cdargs_aliases cv\n"
                    "complete -F _cdargs_aliases cb\n"
                    "garbage\n")
            (let ((rules (make-hash-table :test 'equal)))
              (bash-completion-initialize-rules (current-buffer) rules)
              (mapcar (lambda (cmd)
                        (cons cmd (gethash cmd rules)))
                      (list "cdb" "project" "pro" "cv" "cb"))))
           '(("cdb" "-F" "_cdargs_aliases")
             ("project" "-F" "complete_projects")
             ("pro" "-F" "complete_projects")
             ("cv" "-F" "_cdargs_aliases")
             ("cb" "-F" "_cdargs_aliases")))))

(ert-deftest bash-completion-test-quote-not-necessary ()
  :tags '(bash-completion)
  "bash-completion-quote not necessary"
  (should (string=
           (bash-completion-quote "hello")
           "hello")))

(ert-deftest bash-completion-test-quote-space ()
  :tags '(bash-completion)
  "bash-completion-quote space"
  (should (string=
           (bash-completion-quote "hello world")
           "'hello world'")))

(ert-deftest bash-completion-test-quote-quote ()
  :tags '(bash-completion)
  "bash-completion-quote quote"
  (should (string=
           (bash-completion-quote "hell'o")
           "'hell'\\''o'")))

(ert-deftest bash-completion-test-generate-line-no-custom-completion ()
  :tags '(bash-completion)
  "bash-completion-generate-line no custom completion"
  (should (string=
           (let ((bash-completion-initialized t)
                 (bash-completion-rules (make-hash-table :test 'equal))
                 (default-directory "~/test"))
             (bash-completion-generate-line "hello worl" 7 '("hello" "worl") 1 "worl"))
           (format "compgen -P '%s' -f -- worl" bash-completion-candidates-prefix))))

(ert-deftest bash-completion-test-generate-line-custom-completion-no-function-or-command ()
  :tags '(bash-completion)
  "bash-completion-generate-line custom completion no function or command"
  (should (string=
           (let ((bash-completion-initialized t)
                 (bash-completion-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bash-completion-add-rule (list "complete" "-A" "-G" "*.txt" "zorg") bash-completion-rules)
             (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "compgen -P '%s' -A -G '*.txt' -- worl" bash-completion-candidates-prefix))))

(ert-deftest bash-completion-test-generate-line-custom-completion-function ()
  :tags '(bash-completion)
  "bash-completion-generate-line custom completion function"
  (should (string=
           (let ((bash-completion-initialized t)
                 (bash-completion-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bash-completion-add-rule (list "complete" "-F" "__zorg" "zorg") bash-completion-rules)
             (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -P '%s' -F __bash_complete_wrapper -- worl" bash-completion-candidates-prefix))))

(ert-deftest bash-completion-test-generate-line-custom-completion-command ()
  :tags '(bash-completion)
  "bash-completion-generate-line custom completion command"
  (should (string=
           (let ((bash-completion-initialized t)
                 (bash-completion-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bash-completion-add-rule (list "complete" "-C" "__zorg" "zorg") bash-completion-rules)
             (bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -P '%s' -F __bash_complete_wrapper -- worl" bash-completion-candidates-prefix))))

(ert-deftest bash-completion-test-starts-with-empty-str ()
  :tags '(bash-completion)
  "bash-completion-starts-with empty str"
  (should-not (bash-completion-starts-with "" "prefix")))

(ert-deftest bash-completion-test-starts-with-starts-with ()
  :tags '(bash-completion)
  "bash-completion-starts-with starts with"
  (should (bash-completion-starts-with "blah-blah" "blah-")))

(ert-deftest bash-completion-test-starts-with-does-not-starts-with ()
  :tags '(bash-completion)
  "bash-completion-starts-with does not starts with"
  (should-not (bash-completion-starts-with "blah-blah" "blih-")))

(ert-deftest bash-completion-test-starts-with-same ()
  :tags '(bash-completion)
  "bash-completion-starts-with same"
  (should (bash-completion-starts-with "blah-" "blah-")))

(ert-deftest bash-completion-test-send ()
  :tags '(bash-completion)
  "bash-completion-send"
  (should (string=
           (cl-letf ((bash-completion-initialized t)
                     (process 'something-else)
                     (kill-buffer-query-functions nil)
                     ((symbol-function 'get-buffer-process)
                      (lambda (buffer)
                        'process))
                     ((symbol-function 'process-buffer)
                      (lambda (process)
                        (unless (eq process 'process)
                          (error "unexpected process: %s" process))
                        (current-buffer)))
                     ((symbol-function 'comint-redirect-send-command-to-process)
                      (lambda (command output-buffer process echo no-display)
                        (unless (eq process 'process)
                          (error "unexpected process: %s" process))
                        (unless (string= " cmd" command)
                          (error "unexpected command: %s" command))
                        (setq comint-redirect-completed nil)))
                     ((symbol-function 'accept-process-output)
                      (lambda (process timeout)
                        (insert "line1\nline2\n")
                        (setq comint-redirect-completed t)
                        t)))
             (with-temp-buffer
               (bash-completion-send "cmd" (current-buffer))
               (buffer-string)))
           "line1\nline2\n")))

(ert-deftest bash-completion-test-addsuffix-ends-with-slash ()
  :tags '(bash-completion)
  "bash-completion-addsuffix ends with /"
  (should (string=
           (cl-letf (((symbol-function 'file-accessible-directory-p)
                      (lambda (a)
                        (error "unexpected"))))
             (bash-completion-addsuffix "hello/"))
           "hello/")))

(ert-deftest bash-completion-test-addsuffix-ends-with-space ()
  :tags '(bash-completion)
  "bash-completion-addsuffix ends with space"
  (should (string=
           (cl-letf (((symbol-function 'file-accessible-directory-p)
                      (lambda (a)
                        (error "unexpected"))))
             (bash-completion-addsuffix "hello "))
           "hello ")))

(ert-deftest bash-completion-test-addsuffix-ends-with-separator ()
  :tags '(bash-completion)
  "bash-completion-addsuffix ends with separator"
  (should (string=
           (cl-letf (((symbol-function 'file-accessible-directory-p)
                      (lambda (a)
                        (error "unexpected"))))
             (bash-completion-addsuffix "hello:"))
           "hello:")))

(ert-deftest bash-completion-test-addsuffix-check-directory ()
  :tags '(bash-completion)
  "bash-completion-addsuffix check directory"
  (should (string=
           (cl-letf (((symbol-function 'file-accessible-directory-p)
                      (lambda (a)
                        (string= a
                                 (if (memq system-type '(windows-nt ms-dos))
                                     "c:/tmp/hello"
                                   "/tmp/hello"))))
                     (default-directory
                       (if (memq system-type '(windows-nt ms-dos))
                           "c:/tmp"
                         "/tmp")))
             (bash-completion-addsuffix "hello"))
           "hello/")))

(ert-deftest bash-completion-test-addsuffix-check-directory-expand-tilde ()
  :tags '(bash-completion)
  "bash-completion-addsuffix check directory, expand tilde"
  (should (string=
           (cl-letf (((symbol-function 'file-accessible-directory-p)
                      (lambda (a)
                        (string= a (concat (expand-file-name "y" "~/x")))))
                     (default-directory "~/x"))
             (bash-completion-addsuffix "y"))
           "y/")))

(ert-deftest bash-completion-test-starts-with ()
  :tags '(bash-completion)
  "bash-completion-starts-with"
  (should-not (bash-completion-starts-with "" "hello "))
  (should (bash-completion-starts-with "hello world" "hello "))
  (should-not (bash-completion-starts-with "hello world" "hullo "))
  (should (bash-completion-starts-with "hello" "")))

(ert-deftest bash-completion-test-ends-with ()
  :tags '(bash-completion)
  "bash-completion-ends-with"
  (should-not (bash-completion-ends-with "" "world"))
  (should (bash-completion-ends-with "hello world" "world"))
  (should-not (bash-completion-ends-with "hello world" "wurld"))
  (should (bash-completion-ends-with "hello" "")))

(ert-deftest bash-completion-test-last-wordbreak-split ()
  :tags '(bash-completion)
  "bash-completion-last-wordbreak-split"
  (should (equal (bash-completion-last-wordbreak-split "a:b:c:d:e")
                 '("a:b:c:d:" . "e")))
  (should (equal (bash-completion-last-wordbreak-split "hello=world")
                 '("hello=" . "world")))
  (should (equal (bash-completion-last-wordbreak-split "hello>world")
                 '("hello>" . "world")))
  (should (equal (bash-completion-last-wordbreak-split ">world")
                 '(">" . "world")))
  (should (equal (bash-completion-last-wordbreak-split "hello")
                 '("" . "hello"))))

(ert-deftest bash-completion-test-before-last-wordbreak ()
  :tags '(bash-completion)
  "bash-completion-before-last-wordbreak"
  (should (string= (bash-completion-before-last-wordbreak "a:b:c:d:e") "a:b:c:d:"))
  (should (string= (bash-completion-before-last-wordbreak "hello=world") "hello="))
  (should (string= (bash-completion-before-last-wordbreak "hello>world") "hello>"))
  (should (string= (bash-completion-before-last-wordbreak "hello") "")))

(ert-deftest bash-completion-test-after-last-wordbreak ()
  :tags '(bash-completion)
  "bash-completion-after-last-wordbreak"
  (should (string= (bash-completion-after-last-wordbreak "a:b:c:d:e") "e"))
  (should (string= (bash-completion-after-last-wordbreak "hello=world") "world"))
  (should (string= (bash-completion-after-last-wordbreak "hello>world") "world"))
  (should (string= (bash-completion-after-last-wordbreak "hello") "hello")))

(ert-deftest bash-completion-test-postprocess-escape-rest ()
  :tags '(bash-completion)
  "bash-completion-postprocess escape rest"
  (should (string=
           (bash-completion-postprocess "a\\ bc d e" "a\\ b")
           "a\\ bc\\ d\\ e")))

(ert-deftest bash-completion-test-postprocess-do-not-escape-final-space ()
  :tags '(bash-completion)
  "bash-completion-postprocess do not escape final space"
  (should (string=
           (let ((bash-completion-nospace nil))
             (bash-completion-postprocess "ab " "a"))
           "ab ")))

(ert-deftest bash-completion-test-postprocess-remove-final-space ()
  :tags '(bash-completion)
  "bash-completion-postprocess remove final space"
  (should (string=
           (let ((bash-completion-nospace t))
             (bash-completion-postprocess "ab " "a"))
           "ab")))

(ert-deftest bash-completion-test-postprocess-unexpand-home-and-escape ()
  :tags '(bash-completion)
  "bash-completion-postprocess unexpand home and escape"
  (should (string=
           (bash-completion-postprocess (expand-file-name "~/a/hello world") "~/a/he")
           "~/a/hello\\ world")))

(ert-deftest bash-completion-test-postprocess-match-after-wordbreak-and-escape ()
  :tags '(bash-completion)
  "bash-completion-postprocess match after wordbreak and escape"
  (should (string=
           (bash-completion-postprocess "hello world" "a:b:c:he")
           "a:b:c:hello\\ world")))

(ert-deftest bash-completion-test-postprocess-just-append ()
  :tags '(bash-completion)
  "bash-completion-postprocess just append"
  (should (string=
           (bash-completion-postprocess " world" "hello")
           "hello\\ world")))

(ert-deftest bash-completion-test-postprocess-subset-of-the-prefix ()
  :tags '(bash-completion)
  "bash-completion-postprocess subset of the prefix"
  (should (string=
           (bash-completion-postprocess "Dexter" "Dexter'")
           "Dexter")))

(ert-deftest bash-completion-test-postprocess-for-ending-with-a-slash ()
  :tags '(bash-completion)
  "bash-completion-postprocess for \"~\" ending with a slash"
  (should (string=
           (cl-letf ((real-expand-file-name (symbol-function 'expand-file-name))
                     ((symbol-function 'expand-file-name)
                      (lambda (name &optional default-dir)
                        (if (string= name "~")
                            "/"
                          (funcall real-expand-file-name name default-dir)))))
             (bash-completion-postprocess "/foo/bar" "~/f"))
           "~/foo/bar")))

(ert-deftest bash-completion-test-extract-candidates ()
  :tags '(bash-completion)
  "bash-completion-extract-candidates"
  (should (equal
           (let ((bash-completion-nospace nil))
             (sz-testutils-with-buffer
              (format "%shello world\n%shello \n\n" bash-completion-candidates-prefix bash-completion-candidates-prefix)
              (bash-completion-extract-candidates (current-buffer) "hello" nil)))
           '("hello\\ world" "hello "))))

(ert-deftest bash-completion-test-extract-candidates-with-spurious-output ()
  :tags '(bash-completion)
  "bash-completion-extract-candidates with spurious output"
  (should (equal
           (let ((bash-completion-nospace nil))
             (sz-testutils-with-buffer
              (format "%shello world\nspurious \n\n" bash-completion-candidates-prefix)
              (bash-completion-extract-candidates (current-buffer) "hello" nil)))
           '("hello\\ world"))))

(ert-deftest bash-completion-test-nonsep ()
  :tags '(bash-completion)
  "bash-completion-nonsep"
  (should (string= (bash-completion-nonsep nil) "^ \t\n\r;&|'\"\\\\#"))
  (should (string= (bash-completion-nonsep ?\') "^ \t\n\r'"))
  (should (string= (bash-completion-nonsep ?\") "^ \t\n\r\"\\\\")))

(ert-deftest bash-completion-test-escape-candidate-no-quote ()
  :tags '(bash-completion)
  "bash-completion-escape-candidate no quote"
  (should (string=
           (bash-completion-escape-candidate "He said: \"hello, 'you'\"" nil)
           "He\\ said:\\ \\\"hello,\\ \\'you\\'\\\""))
  (should (string=
           (bash-completion-escape-candidate "#hello#" nil)
           "\\#hello\\#")))

(ert-deftest bash-completion-test-escape-candidate-single-quote ()
  :tags '(bash-completion)
  "bash-completion-escape-candidate single quote"
  (should (string=
           (bash-completion-escape-candidate "He said: \"hello, 'you'\"" 39)
           "He said: \"hello, '\\''you'\\''\"")))

(ert-deftest bash-completion-test-escape-candidate-double-quote ()
  :tags '(bash-completion)
  "bash-completion-escape-candidate double quote"
  (should (string=
           (bash-completion-escape-candidate "He said: \"hello, 'you'\"" 34)
           "He said: \\\"hello, 'you'\\\"")))

(ert-deftest bash-completion-test-escape-candidate-no-quote-not-if-double-quoted ()
  :tags '(bash-completion)
  "bash-completion-escape-candidate no quote not if double quoted"
  (should (string=
           (bash-completion-escape-candidate "\"hello, you" nil)
           "\"hello, you")))

(ert-deftest bash-completion-test-escape-candidate-no-quote-not-if-single-quoted ()
  :tags '(bash-completion)
  "bash-completion-escape-candidate no quote not if single quoted"
  (should (string=
           (bash-completion-escape-candidate "'hello, you" nil)
           "'hello, you")))

(ert-deftest bash-completion-test-quote-allowed ()
  :tags '(bash-completion)
  "bash-completion-quote allowed"
  (should (string=
           (bash-completion-quote "abc_ABC/1-2.3")
           "abc_ABC/1-2.3")))

(ert-deftest bash-completion-test-quote-quoted ()
  :tags '(bash-completion)
  "bash-completion-quote quoted"
  (should (string=
           (bash-completion-quote "a$b")
           "'a$b'")))

(ert-deftest bash-completion-test-quote-quoted-single-quote ()
  :tags '(bash-completion)
  "bash-completion-quote quoted single quote"
  (should (string=
           (bash-completion-quote "a'b")
           "'a'\\''b'")))

(ert-deftest bash-completion-test-join ()
  :tags '(bash-completion)
  "bash-completion-join"
  (should (string=
           (bash-completion-join '("ls" "-l" "/a/b" "/a/b c" "/a/b'c" "$help/d"))
           "ls -l /a/b '/a/b c' '/a/b'\\''c' '$help/d'")))

(ert-deftest bash-completion-test-completion-in-region ()
  :tags '(bash-completion)
  "Simple tests for `completion-in-region'."
  (should (string=
           (sz-testutils-with-buffer
            '("f" cursor "b")
            (let ((completion-styles '(basic)))
              (completion-in-region (point-min) (point-max) '("foo-bar" "fox" "fun")))
            (buffer-string))
           "foo-bar"))

  (should (string=
           (sz-testutils-with-buffer
            '("f" cursor "n")
            (let ((completion-styles '(basic)))
              (completion-in-region (point-min) (point-max) '("foo-bar" "fox" "fun")))
            (buffer-string))
           "fun"))


  (should (string=
           (sz-testutils-with-buffer
            '("f-" cursor "b")
            (let ((completion-styles '(basic)))
              (completion-in-region (point-min) (point-max) '("foo-bar" "fox" "fun")))
            (buffer-string))
           "f-b"))

  (should (string=
           (sz-testutils-with-buffer
            '("f-" cursor "b")
            (let ((completion-styles '(partial-completion)))
              (completion-in-region (point-min) (point-max) '("foo-bar" "fox" "fun")))
            (buffer-string))
           "foo-bar")))


(defmacro bash-completion-tests-with-shell (&rest body)
  (let ((shell-buffer (make-symbol "shell-buffer")))
    `(let ((,shell-buffer nil))
       (unwind-protect
           (progn
             (setq ,shell-buffer (generate-new-buffer "*bash-completion-tests-with-shell*"))
             (shell ,shell-buffer)
             (with-current-buffer ,shell-buffer
               (while (accept-process-output nil 1))
               (goto-char (point-max)) 
               (let ((start (point)))
                 ,@body)))
         (when ,shell-buffer
           (when (buffer-live-p ,shell-buffer)
             (comint-send-string ,shell-buffer "\nexit\n")
             (sit-for 1))
           (kill-buffer ,shell-buffer))))))

(ert-deftest bash-completion-test-interaction ()
  :tags '(bash-completion-integration)
  "bash-completion interaction"
  (should-not bash-completion-initialized)
  (should-not (hash-table-p bash-completion-rules))
  (should (member "help "
                  (bash-completion-tests-with-shell
                   (bash-completion-comm "hel" 4 '("hel") 0 "hel" nil)))))

(ert-deftest bash-completion-test-execute-one-completion ()
  :tags '(bash-completion-integration)
  "bash-completion execute one completion"
  (should (equal (bash-completion-tests-with-shell
                  (let ((pos (point)))
                    (insert "__bash_complete_")
                    (bash-completion-dynamic-complete)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "__bash_complete_wrapper ")))

(ert-deftest bash-completion-test-execute-wordbreak-completion ()
  :tags '(bash-completion-integration)
  "bash-completion execute wordbreak completion"
  (should (equal (bash-completion-tests-with-shell
                  (let ((pos (point)))
                    (insert "export PATH=/sbin:/bi")
                    (bash-completion-dynamic-complete)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "export PATH=/sbin:/bin")))

(provide 'bash-completion-tests)
;;; bash-completion-tests.el ends here

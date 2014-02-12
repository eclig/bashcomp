;;; bashcomp-tests.el --- Tests cases for bashcomp.el

;; Copyright (C) 2013, 2014 Emílio Lopes
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
;; This file defines regression tests for the "bashcomp" package.
;; Eval these forms to run the tests:
;;
;;     (ert '(tag bashcomp))
;;     (ert '(tag bashcomp-integration))
;;
;; See Info(ert) for more information.

;;; Code:

(require 'ert)
(require 'bashcomp)
(require 'sz-testutils)

(eval-when-compile
  '(require cl-macs))

(ert-deftest bashcomp-test-tokenize-simple ()
  :tags '(bashcomp)
  "bashcomp-tokenize simple"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello world b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello" "world" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-simple-extra-spaces ()
  :tags '(bashcomp)
  "bashcomp-tokenize simple extra spaces"
  (should (equal
           (sz-testutils-with-buffer
            '("  a  hello \n world \t b \r c  ")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position 2))))
           '("a" "hello" "world" "b" "c" ""))))

(ert-deftest bashcomp-test-tokenize-escaped-char ()
  :tags '(bashcomp)
  "bashcomp-tokenize escaped char"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello\\-world b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello-world" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-escaped-space ()
  :tags '(bashcomp)
  "bashcomp-tokenize escaped space"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello\\ world b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-escaped-hash ()
  :tags '(bashcomp)
  "bashcomp-tokenize escaped #"
  (should (equal
           (sz-testutils-with-buffer
            '("a hello \\#world\\# b")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello" "#world#" "b"))))

(ert-deftest bashcomp-test-tokenize-double-quotes ()
  :tags '(bashcomp)
  "bashcomp-tokenize double quotes"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"hello world\" b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-double-quotes-escaped ()
  :tags '(bashcomp)
  "bashcomp-tokenize double quotes escaped"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"-\\\"hello world\\\"-\" b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "-\"hello world\"-" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-single-quotes ()
  :tags '(bashcomp)
  "bashcomp-tokenize single quotes"
  (should (equal
           (sz-testutils-with-buffer
            '("a \"hello world\" b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello world" "b" "c"))))

(ert-deftest bashcomp-test-tokenize-single-quotes-escaped ()
  :tags '(bashcomp)
  "bashcomp-tokenize single quotes escaped"
  (should (equal
           (sz-testutils-with-buffer
            '("a '-\\'hello world\\'-' b c")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "-\\hello" "world'- b c"))))

(ert-deftest bashcomp-test-get-token-open-single-quote ()
  :tags '(bashcomp)
  "bashcomp-tokenize with a single quote open"
  (should (string=
           (sz-testutils-with-buffer
            '("hello 'world")
            ;; 123456789
            (goto-char 7)
            (bashcomp-token-string
             (bashcomp-get-token (line-end-position))))
           "world")))

(ert-deftest bashcomp-test-tokenize-open-single-quote-limited ()
  :tags '(bashcomp)
  "bashcomp-tokenize with a single quote open limited"
  (should (string=
           (sz-testutils-with-buffer
            '("hello 'world")
            ;; 123456789
            (goto-char 7)
            (bashcomp-token-string
             (bashcomp-get-token 10)))
           "wo")))

(ert-deftest bashcomp-test-tokenize-complex-quote-mix ()
  :tags '(bashcomp)
  "bashcomp-tokenize complex quote mix"
  (should (equal
           (sz-testutils-with-buffer
            '("a hel\"lo w\"o'rld b'c d")
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("a" "hello world bc" "d"))))

(ert-deftest bashcomp-test-tokenize-unescaped-semicolon ()
  :tags '(bashcomp)
  "bashcomp-tokenize unescaped semicolon"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity;and\\ beyond"
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("to" "infinity" ";" "and beyond"))))

(ert-deftest bashcomp-test-tokenize-unescaped-and ()
  :tags '(bashcomp)
  "bashcomp-tokenize unescaped &&"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity&&and\\ beyond"
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("to" "infinity" "&&" "and beyond"))))

(ert-deftest bashcomp-test-tokenize-unescaped-or ()
  :tags '(bashcomp)
  "bashcomp-tokenize unescaped ||"
  (should (equal
           (sz-testutils-with-buffer
            "to infinity||and\\ beyond"
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("to" "infinity" "||" "and beyond"))))

(ert-deftest bashcomp-test-tokenize-quoted-separators ()
  :tags '(bashcomp)
  "bashcomp-tokenize quoted ;&|"
  (should (equal
           (sz-testutils-with-buffer
            "to \"infinity;&|and\" beyond"
            (mapcar 'bashcomp-token-string
                    (bashcomp-tokenize 1 (line-end-position))))
           '("to" "infinity;&|and" "beyond"))))

(ert-deftest bashcomp-test-parse-line-empty-line ()
  :tags '(bashcomp)
  "The empty line should lead to a single empty token."
  (should (equal
           (sz-testutils-with-buffer
            ""
            (bashcomp-parse-line 1 (line-end-position)))
           '("" 0 0 "" ("")))))

(ert-deftest bashcomp-test-parse-line-trailing-space ()
  :tags '(bashcomp)
  "The trailing space should lead to a new (empty) token at the end of the line."
  (should (equal
           (sz-testutils-with-buffer
            "cd "
            (bashcomp-parse-line 1 (line-end-position)))
           '("cd " 3 1 "" ("cd" "")))))

(ert-deftest bashcomp-test-parse-line-cursor-at-end-of-word ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor at end of word"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world"
            (bashcomp-parse-line 1 (line-end-position)))
           '("a hello world" 13 2 "world" ("a" "hello" "world")))))

(ert-deftest bashcomp-test-parse-line-cursor-in-the-middle-of-a-word ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor in the middle of a word"
  (should (equal
           (sz-testutils-with-buffer
            "a hello wo"
            (bashcomp-parse-line 1 (line-end-position)))
           '("a hello wo" 10 2 "wo" ("a" "hello" "wo")))))

(ert-deftest bashcomp-test-parse-line-cursor-at-the-beginning ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor at the beginning"
  (should (equal
           (sz-testutils-with-buffer
            " "
            (bashcomp-parse-line 1 (line-end-position)))
           '("" 0 0 "" ("")))))

(ert-deftest bashcomp-test-parse-line-cursor-in-the-middle ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor in the middle"
  (should (equal
           (sz-testutils-with-buffer
            "a hello "
            (bashcomp-parse-line 1 (line-end-position)))
           '("a hello " 8 2 "" ("a" "hello" "")))))

(ert-deftest bashcomp-test-parse-line-cursor-at-end ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor at end"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world b c"
            (bashcomp-parse-line 1 (line-end-position)))
           '("a hello world b c" 17 4 "c" ("a" "hello" "world" "b" "c")))))

(ert-deftest bashcomp-test-parse-line-complex-multi-command-line ()
  :tags '(bashcomp)
  "bashcomp-parse-line complex multi-command line"
  (should (equal
           (sz-testutils-with-buffer
            "cd /var/tmp ; ZORG=t make "
            (bashcomp-parse-line 1 (line-end-position)))
           '("make " 5 1 "" ("make" "")))))

(ert-deftest bashcomp-test-parse-line-pipe ()
  :tags '(bashcomp)
  "bashcomp-parse-line pipe"
  (should (equal
           (sz-testutils-with-buffer
            "ls /var/tmp | sort "
            (bashcomp-parse-line 1 (line-end-position)))
           '("sort " 5 1 "" ("sort" "")))))

(ert-deftest bashcomp-test-parse-line-escaped-semicolon ()
  :tags '(bashcomp)
  "bashcomp-parse-line escaped semicolon"
  (should (equal
           (sz-testutils-with-buffer
            "find -name '*.txt' -exec echo {} ';' -"
            (bashcomp-parse-line 1 (line-end-position)))
           '("find -name '*.txt' -exec echo {} ';' " 37 7 ""
             ("find" "-name" "*.txt" "-exec" "echo" "{}" ";" "")))))

(ert-deftest bashcomp-test-parse-line-at-var-assignment ()
  :tags '(bashcomp)
  "bashcomp-parse-line at var assignment"
  (should (equal
           (sz-testutils-with-buffer
            "cd /var/tmp ; A=f ZORG=t"
            (bashcomp-parse-line 1 (line-end-position)))
           '("ZORG=t" 6 0 "ZORG=t" ("ZORG=t")))))

(ert-deftest bashcomp-test-parse-line-cursor-after-end ()
  :tags '(bashcomp)
  "bashcomp-parse-line cursor after end"
  (should (equal
           (sz-testutils-with-buffer
            "a hello world b c "
            (bashcomp-parse-line 1 (line-end-position)))
           '("a hello world b c " 18 5 ""
             ("a" "hello" "world" "b" "c" "")))))

;; broken on system-type `windows-nt' because Emacs considers the
;; backslash to be a directory separator on those systems.
;;
;; (ert-deftest bashcomp-test-parse-line-with-escaped-quote ()
;;   :tags '(bashcomp)
;;   "bashcomp-parse-line with escaped quote"
;;   (should (equal
;;            (sz-testutils-with-buffer
;;             "cd /vcr/shows/Dexter\\'s"
;;             (bashcomp-parse-line 1 (line-end-position)))
;;            '("cd /vcr/shows/Dexter\\'s" 23 1 "/vcr/shows/Dexter's"
;;              ("cd" "/vcr/shows/Dexter's")))))

(ert-deftest bashcomp-test-add-rule-garbage ()
  :tags '(bashcomp)
  "bashcomp-add-rule garbage"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bashcomp-add-rule (list "just" "some" "garbage") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bashcomp-test-add-rule-empty ()
  :tags '(bashcomp)
  "bashcomp-add-rule empty"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bashcomp-add-rule nil rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bashcomp-test-add-rule-empty-string ()
  :tags '(bashcomp)
  "bashcomp-add-rule empty string"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bashcomp-add-rule (list "") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bashcomp-test-add-rule-empty-complete ()
  :tags '(bashcomp)
  "bashcomp-add-rule empty complete"
  (should (let ((rules (make-hash-table :test 'equal)))
            (bashcomp-add-rule (list "complete") rules)
            (zerop (hash-table-count rules)))))

(ert-deftest bashcomp-test-add-rule-one-command ()
  :tags '(bashcomp)
  "bashcomp-add-rule one command"
  (should (equal
           (let ((rules (make-hash-table :test 'equal)))
             (bashcomp-add-rule (list "complete" "-e" "-F" "_cdargs_aliases" "cdb") rules)
             (gethash "cdb" rules))
           '("-e" "-F" "_cdargs_aliases"))))

(ert-deftest bashcomp-test-initialize-rules ()
  :tags '(bashcomp)
  "bashcomp-initialize-rules"
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
              (bashcomp-initialize-rules (current-buffer) rules)
              (mapcar (lambda (cmd)
                        (cons cmd (gethash cmd rules)))
                      (list "cdb" "project" "pro" "cv" "cb"))))
           '(("cdb" "-F" "_cdargs_aliases")
             ("project" "-F" "complete_projects")
             ("pro" "-F" "complete_projects")
             ("cv" "-F" "_cdargs_aliases")
             ("cb" "-F" "_cdargs_aliases")))))

(ert-deftest bashcomp-test-quote-not-necessary ()
  :tags '(bashcomp)
  "bashcomp-quote not necessary"
  (should (string=
           (bashcomp-quote "hello")
           "hello")))

(ert-deftest bashcomp-test-quote-space ()
  :tags '(bashcomp)
  "bashcomp-quote space"
  (should (string=
           (bashcomp-quote "hello world")
           "'hello world'")))

(ert-deftest bashcomp-test-quote-quote ()
  :tags '(bashcomp)
  "bashcomp-quote quote"
  (should (string=
           (bashcomp-quote "hell'o")
           "'hell'\\''o'")))

(ert-deftest bashcomp-test-generate-line-no-custom-completion ()
  :tags '(bashcomp)
  "bashcomp-generate-line no custom completion"
  (should (string=
           (let ((bashcomp-initialized t)
                 (bashcomp-rules (make-hash-table :test 'equal))
                 (default-directory "~/test"))
             (bashcomp-generate-line "hello worl" 7 '("hello" "worl") 1 "worl"))
           (format "compgen -P '%s' -f -- worl 2>/dev/null" bashcomp-candidates-prefix))))

(ert-deftest bashcomp-test-generate-line-custom-completion-no-function-or-command ()
  :tags '(bashcomp)
  "bashcomp-generate-line custom completion no function or command"
  (should (string=
           (let ((bashcomp-initialized t)
                 (bashcomp-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bashcomp-add-rule (list "complete" "-A" "-G" "*.txt" "zorg") bashcomp-rules)
             (bashcomp-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "compgen -P '%s' -A -G '*.txt' -- worl 2>/dev/null" bashcomp-candidates-prefix))))

(ert-deftest bashcomp-test-bashcomp-specification ()
  :tags '(bashcomp)
  "bashcomp-specification"
  (let ((bashcomp-initialized t)
        (bashcomp-rules (make-hash-table :test 'equal))
        (default-directory "/test"))
    (bashcomp-add-rule (list "complete" "-A" "-G" "*.txt" "zorg") bashcomp-rules)
    (let ((system-type 'toto))
      (should
       (equal (bashcomp-specification "zorg")
              '("-A" "-G" "*.txt")))
      (should
       (equal (bashcomp-specification "/bin/zorg")
              '("-A" "-G" "*.txt")))
      (should-not (bashcomp-specification "zorg.exe"))
      (should-not (bashcomp-specification "/bin/zorg.exe")))

    (let ((system-type 'windows-nt))
      (should
       (equal (bashcomp-specification "zorg")
              '("-A" "-G" "*.txt")))
      (should-not (bashcomp-specification "zorgo.exe"))
      (should
       (equal (bashcomp-specification "zorg.exe")
              '("-A" "-G" "*.txt")))
      (should
       (equal (bashcomp-specification "c:\\foo\\bar\\zorg.exe")
              '("-A" "-G" "*.txt"))))))

(ert-deftest bashcomp-test-generate-line-custom-completion-function ()
  :tags '(bashcomp)
  "bashcomp-generate-line custom completion function"
  (should (string=
           (let ((bashcomp-initialized t)
                 (bashcomp-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bashcomp-add-rule (list "complete" "-F" "__zorg" "zorg") bashcomp-rules)
             (bashcomp-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -P '%s' -F __bash_complete_wrapper -- worl 2>/dev/null" bashcomp-candidates-prefix))))

(ert-deftest bashcomp-test-generate-line-custom-completion-command ()
  :tags '(bashcomp)
  "bashcomp-generate-line custom completion command"
  (should (string=
           (let ((bashcomp-initialized t)
                 (bashcomp-rules (make-hash-table :test 'equal))
                 (default-directory "/test"))
             (bashcomp-add-rule (list "complete" "-C" "__zorg" "zorg") bashcomp-rules)
             (bashcomp-generate-line "zorg worl" 7 '("zorg" "worl") 1 "worl"))
           (format "__BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -P '%s' -F __bash_complete_wrapper -- worl 2>/dev/null" bashcomp-candidates-prefix))))

(ert-deftest bashcomp-test-send ()
  :tags '(bashcomp)
  "bashcomp-send"
  (should (string=
           (cl-letf ((bashcomp-initialized t)
                     (process 'something-else)
                     (kill-buffer-query-functions nil)
                     ((symbol-function 'get-buffer-process)
                      (lambda (buffer)
                        'process))
                     ((symbol-function 'processp)
                      (lambda (process)
                        (eq process 'process)))
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
               (bashcomp-send "cmd" 'process (current-buffer))
               (buffer-string)))
           "line1\nline2\n")))

(ert-deftest bashcomp-test-extract-candidates ()
  :tags '(bashcomp)
  "bashcomp-extract-candidates"
  (should (equal
           (sz-testutils-with-buffer
            (format "%shello world\n%shello \n\n" bashcomp-candidates-prefix bashcomp-candidates-prefix)
            (bashcomp-extract-candidates (current-buffer)))
           '("hello world" "hello"))))

(ert-deftest bashcomp-test-extract-candidates-with-spurious-output ()
  :tags '(bashcomp)
  "bashcomp-extract-candidates with spurious output"
  (should (equal
           (sz-testutils-with-buffer
            (format "%shello world\nspurious \n\n" bashcomp-candidates-prefix)
            (bashcomp-extract-candidates (current-buffer)))
           '("hello world"))))

(ert-deftest bashcomp-test-nonsep ()
  :tags '(bashcomp)
  "bashcomp-nonsep"
  (should (string= (bashcomp-nonsep nil) "^ \t\n\r;&|'\"\\\\#"))
  (should (string= (bashcomp-nonsep ?\') "^ \t\n\r'"))
  (should (string= (bashcomp-nonsep ?\") "^ \t\n\r\"\\\\")))

(ert-deftest bashcomp-test-escape-candidate-no-quote ()
  :tags '(bashcomp)
  "bashcomp-escape-candidate no quote"
  (should (string=
           (bashcomp-escape-candidate "He said: \"hello, 'you'\"" nil)
           "He\\ said:\\ \\\"hello,\\ \\'you\\'\\\""))
  (should (string=
           (bashcomp-escape-candidate "#hello#" nil)
           "\\#hello\\#")))

(ert-deftest bashcomp-test-escape-candidate-single-quote ()
  :tags '(bashcomp)
  "bashcomp-escape-candidate single quote"
  (should (string=
           (bashcomp-escape-candidate "He said: \"hello, 'you'\"" 39)
           "He said: \"hello, '\\''you'\\''\"")))

(ert-deftest bashcomp-test-escape-candidate-double-quote ()
  :tags '(bashcomp)
  "bashcomp-escape-candidate double quote"
  (should (string=
           (bashcomp-escape-candidate "He said: \"hello, 'you'\"" 34)
           "He said: \\\"hello, 'you'\\\"")))

(ert-deftest bashcomp-test-escape-candidate-no-quote-not-if-double-quoted ()
  :tags '(bashcomp)
  "bashcomp-escape-candidate no quote not if double quoted"
  (should (string=
           (bashcomp-escape-candidate "\"hello, you" nil)
           "\"hello, you")))

(ert-deftest bashcomp-test-escape-candidate-no-quote-not-if-single-quoted ()
  :tags '(bashcomp)
  "bashcomp-escape-candidate no quote not if single quoted"
  (should (string=
           (bashcomp-escape-candidate "'hello, you" nil)
           "'hello, you")))

(ert-deftest bashcomp-test-quote-allowed ()
  :tags '(bashcomp)
  "bashcomp-quote allowed"
  (should (string=
           (bashcomp-quote "abc_ABC/1-2.3")
           "abc_ABC/1-2.3")))

(ert-deftest bashcomp-test-quote-quoted ()
  :tags '(bashcomp)
  "bashcomp-quote quoted"
  (should (string=
           (bashcomp-quote "a$b")
           "'a$b'")))

(ert-deftest bashcomp-test-quote-quoted-single-quote ()
  :tags '(bashcomp)
  "bashcomp-quote quoted single quote"
  (should (string=
           (bashcomp-quote "a'b")
           "'a'\\''b'")))

(ert-deftest bashcomp-test-completion-in-region ()
  :tags '(bashcomp)
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


(defmacro bashcomp-tests-with-shell (&rest body)
  (let ((shell-buffer (make-symbol "shell-buffer")))
    `(let ((,shell-buffer nil))
       (unwind-protect
           (progn
             (setq ,shell-buffer (generate-new-buffer "*bashcomp-tests-with-shell*"))
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

(ert-deftest bashcomp-test-interaction ()
  :tags '(bashcomp-integration)
  "bashcomp interaction"
  (should-not bashcomp-initialized)
  (should (hash-table-p bashcomp-rules))
  (should (member "help"
                  (bashcomp-tests-with-shell
                   (let ((process (get-buffer-process (current-buffer))))
                     (bashcomp-initialize process)
                     (bashcomp-generate-completions
                      process 
                      (bashcomp-generate-line "hel" 4 '("hel") 0 "hel")
                      "hel"))))))

(ert-deftest bashcomp-test-execute-one-completion ()
  :tags '(bashcomp-integration)
  "bashcomp execute one completion"
  (should (equal (bashcomp-tests-with-shell
                  (let ((pos (point))
                        (completion-at-point-functions '(bashcomp-completion-at-point)))
                    (insert "__bash_complete_")
                    (completion-at-point)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "__bash_complete_wrapper ")))

(ert-deftest bashcomp-test-execute-wordbreak-completion ()
  :tags '(bashcomp-integration)
  "bashcomp execute wordbreak completion"
  (should (equal (bashcomp-tests-with-shell
                  (let ((pos (point))
                        (completion-at-point-functions '(bashcomp-completion-at-point)))
                    (insert "export PATH=/sbin:/bi")
                    (completion-at-point)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "export PATH=/sbin:/bin")))

(ert-deftest bashcomp-test-completion-with-custom-rule ()
  :tags '(bashcomp-integration)
  "bashcomp completion with custom rule"
  (should (equal (bashcomp-tests-with-shell
                  (comint-send-string (current-buffer) "complete -W 'abc aeiou' foo\n")
                  (sit-for 1)
                  (let ((pos (point))
                        (completion-at-point-functions '(bashcomp-completion-at-point)))
                    (insert "foo ab")
                    (completion-at-point)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "foo abc ")))

(ert-deftest bashcomp-test-completion-with-restart ()
  :tags '(bashcomp-integration)
  "bashcomp completion with restart"
  (should (equal (bashcomp-tests-with-shell
                  (comint-send-string (current-buffer) "complete -F _foo foo; _foo () { complete -W 'abc aeiou' foo; return 124; }\n")
                  (sit-for 1)
                  (let ((pos (point))
                        (completion-at-point-functions '(bashcomp-completion-at-point)))
                    (insert "foo ab")
                    (completion-at-point)
                    (sit-for 1)
                    (buffer-substring-no-properties pos (point))))
                 "foo abc ")))

(provide 'bashcomp-tests)
;;; bashcomp-tests.el ends here

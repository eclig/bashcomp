;;; bash-completion_test.el --- Tests bash-completion.el


;;; Commentary:
;;
;; This file defines `bash-completion-regress' and run the
;; regression tests if and only if regress is already imported.
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  ;; force reload
  (load-library "~/.emacs.d/bash-completion.el")

  (require 'sz-testutils)
  (require 'cl)

  (defvar bash-completion-run-integration-tests nil
    "Run integration tests. Integration start subprocess (bash
shells) and as a result are too slow to be run in many
cases. That's why they need to be enabled manually.")

  ;; This code will not appear in the compiled (.elc) file

  ;; ---------- unit tests
  (put 'bash-completion-regress 'regression-suite t)
  (setq bash-completion-regress
   '("bash-completion-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did

     ("bash-completion-join simple"
      (bash-completion-join '("a" "hello" "world" "b" "c"))
      "a hello world b c")

     ("bash-completion-join escape quote"
      (bash-completion-join '("a" "hel'lo" "world" "b" "c"))
      "a 'hel'\\''lo' world b c")

     ("bash-completion-join escape space"
      (bash-completion-join '("a" "hello world" "b" "c"))
      "a 'hello world' b c")

     ("bash-completion-split simple"
      (sz-testutils-with-buffer
       '("a hello world b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split simple extra spaces"
      (sz-testutils-with-buffer
       '("  a  hello \n world \t b \r c  ")
       (bash-completion-split 1 (line-end-position 2) nil))
      '(nil . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split escaped space"
      (sz-testutils-with-buffer
       '("a hello\\ world b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split double quotes"
      (sz-testutils-with-buffer
       '("a \"hello world\" b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split double quotes escaped"
      (sz-testutils-with-buffer
       '("a \"-\\\"hello world\\\"-\" b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "-\"hello world\"-" "b" "c")))

     ("bash-completion-split single quotes"
      (sz-testutils-with-buffer
       '("a \"hello world\" b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "hello world" "b" "c")))

     ("bash-completion-split single quotes escaped"
      (sz-testutils-with-buffer
       '("a '-\\'hello world\\'-' b c")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "-'hello world'-" "b" "c")))

     ("bash-completion-split complex quote mix"
      (sz-testutils-with-buffer
       '("a hel\"lo w\"o'rld b'c d")
       (bash-completion-split 1 (line-end-position) nil))
      '(nil . ("a" "hello world bc" "d")))

     ("bash-completion-split cursor at end of word"
      (sz-testutils-with-buffer
       '("a hello world" cursor " b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split cursor in the middle of a word"
      (sz-testutils-with-buffer
       '("a hello wo" cursor "rld b c")
       (bash-completion-split 1 (line-end-position) (point)))
      '(2 . ("a" "hello" "world" "b" "c")))

;;      ("bash-completion-split cursor at the beginning"
;;       (sz-testutils-with-buffer
;;        '(" " cursor " a hello world b c")
;;        (bash-completion-split 1 (line-end-position) (point)))
;;       '(0 . ("" "a" "hello" "world" "b" "c")))

;;      ("bash-completion-split cursor in the middle"
;;       (sz-testutils-with-buffer
;;        '("a hello " cursor " world b c")
;;        (bash-completion-split 1 (line-end-position) (point)))
;;       '(2 . ("a" "hello" "" "world" "b" "c")))

     ("bash-completion-split cursor at end"
      (sz-testutils-with-buffer
       '("a hello world b c" cursor)
       (bash-completion-split 1 (line-end-position) (point)))
      '(4 . ("a" "hello" "world" "b" "c")))

     ("bash-completion-split cursor after end"
      (sz-testutils-with-buffer
       '("a hello world b c " cursor)
       (bash-completion-split 1 (line-end-position) (point)))
      '(5 . ("a" "hello" "world" "b" "c" "")))

     ("bash-completion-split with escaped quote"
      (sz-testutils-with-buffer
       '("cd /vcr/shows/Dexter\\'" cursor)
       (bash-completion-split 1 (line-end-position) (point)))
      '(1 . ("cd" "/vcr/shows/Dexter'")))

     ("bash-completion-add-to-alist garbage"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("just" "some" "garbage")))
      nil)

     ("bash-completion-add-to-alist empty"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '()))
      nil)

     ("bash-completion-add-to-alist empty string"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("")))
      nil)

     ("bash-completion-add-to-alist empty complete"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("complete")))
      nil)

     ("bash-completion-add-to-alist one command"
      (let ((bash-completion-alist nil))
	(bash-completion-add-to-alist '("complete" "-e" "-F" "_cdargs_aliases" "cdb")))
      '(("cdb" . ("-e" "-F" "_cdargs_aliases"))))

     ("bash-completion-build-alist"
      (sz-testutils-with-buffer
       "
complete -F _cdargs_aliases cdb
complete -F complete_projects project
complete -F complete_projects pro
complete -F _cdargs_aliases cv
complete -F _cdargs_aliases cb
garbage
"
       (let ((bash-completion-alist '(garbage)))
	 (bash-completion-build-alist (current-buffer))))
      '(("cdb" "-F" "_cdargs_aliases")
	("project" "-F" "complete_projects")
	("pro" "-F" "complete_projects")
	("cv" "-F" "_cdargs_aliases")
	("cb" "-F" "_cdargs_aliases")))

     ("bash-completion-quote not necessary"
      (bash-completion-quote "hello")
      "hello")

     ("bash-completion-quote space"
      (bash-completion-quote "hello world")
      "'hello world'")

     ("bash-completion-quote quote"
      (bash-completion-quote "hell'o")
      "'hell'\\''o'")

     ("bash-completion-generate-line no custom completion"
      (let ((bash-completion-alist nil)
	    (default-directory "~/test"))
	(bash-completion-generate-line "hello worl" 7 '("hello" "worl") 1))
      (concat "cd 2>/dev/null " (expand-file-name "~/test") " ; compgen -o default worl"))

     ("bash-completion-generate-line custom completion no function or command"
      (let ((bash-completion-alist '(("zorg" . ("-A" "-G" "*.txt"))))
	    (default-directory "/test"))
	(bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "cd 2>/dev/null /test ; compgen -A -G '*.txt' -- worl")

     ("bash-completion-generate-line custom completion function"
      (let ((bash-completion-alist '(("zorg" . ("-F" "__zorg"))))
	    (default-directory "/test"))
	(bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "cd 2>/dev/null /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -F __bash_complete_wrapper -- worl")

     ("bash-completion-generate-line custom completion command"
      (let ((bash-completion-alist '(("zorg" . ("-C" "__zorg"))))
	    (default-directory "/test"))
	(bash-completion-generate-line "zorg worl" 7 '("zorg" "worl") 1))
      "cd 2>/dev/null /test ; __BASH_COMPLETE_WRAPPER='COMP_LINE='\\''zorg worl'\\''; COMP_POINT=7; COMP_CWORD=1; COMP_WORDS=( zorg worl ); __zorg \"${COMP_WORDS[@]}\"' compgen -F __bash_complete_wrapper -- worl")

     ("bash-completion-line-beginning-position start"
      (sz-testutils-with-buffer
       "cd /home/x"
       (bash-completion-line-beginning-position 1))
      1)

     ("bash-completion-line-beginning-position semicolon"
      (sz-testutils-with-buffer
       '("cd /home/x ; " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(14 14))

     ("bash-completion-line-beginning-position 2 semicolon"
      (sz-testutils-with-buffer
       '("cd /home/x ; blah; " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(20 20))

     ("bash-completion-line-beginning-position &&"
      (sz-testutils-with-buffer
       '("cd /home/x && " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(15 15))

     ("bash-completion-line-beginning-position ||"
      (sz-testutils-with-buffer
       '("cd /home/x || " cursor "echo hello")
       (list
	(point)
	(bash-completion-line-beginning-position 1)))
      '(15 15))

     ("bash-completion-starts-with empty str"
      (bash-completion-starts-with "" "prefix")
      nil)

     ("bash-completion-starts-with starts with"
      (bash-completion-starts-with "blah-blah" "blah-")
      t)

     ("bash-completion-starts-with does not starts with"
      (bash-completion-starts-with "blah-blah" "blih-")
      nil)

     ("bash-completion-starts-with same"
      (bash-completion-starts-with "blah-" "blah-")
      t)

     ("bash-completion-send"
      (let ((process 'proces))
	(flet ((process-buffer
		(process)
		(unless (eq process 'process)
		  (error "unexpected: %s" process))
		(current-buffer))
	       (process-send-string
		(process command)
		(unless (eq process 'process)
		  (error "unexpected process: %s" process))
		(unless (equal "cmd\n" command)
		  (error "unexpected command: %s" command)))
	       (accept-process-output
		(process timeout)
		(unless (eq process 'process)
		  (error "unexpected process: %s" process))
		(unless (= timeout 3.14)
		  (error "unexpected timeout: %s" timeout))
		(insert "line1\nline2\n\v")
		t))
	  (sz-testutils-with-buffer-content
	   ""
	   (bash-completion-send "cmd" 'process 3.14))))
	  "line1\nline2\n")

     ("bash-completion-cd-command-prefix no current dir"
      (let ((default-directory nil))
	(bash-completion-cd-command-prefix))
      "")

     ("bash-completion-cd-command-prefix current dir"
      (let ((default-directory "/tmp/x"))
	(bash-completion-cd-command-prefix))
      "cd 2>/dev/null /tmp/x ; ")

     ("bash-completion-cd-command-prefix expand tilde"
      (let ((default-directory "~/x"))
	(bash-completion-cd-command-prefix))
      (concat "cd 2>/dev/null " (expand-file-name "~/x") " ; "))

     ("bash-completion-addsuffix ends with /"
      (flet ((file-accessible-directory-p (a) (error "unexpected")))
	(bash-completion-addsuffix "hello/"))
      "hello/")

     ("bash-completion-addsuffix ends with space"
      (flet ((file-accessible-directory-p (a) (error "unexpected")))
	(bash-completion-addsuffix "hello "))
      "hello ")

     ("bash-completion-addsuffix ends with separator"
      (flet ((file-accessible-directory-p (a) (error "unexpected")))
	(bash-completion-addsuffix "hello:"))
      "hello:")

     ("bash-completion-addsuffix check directory"
      (flet ((file-accessible-directory-p (a) (equal a "/tmp/hello")))
	(let ((default-directory "/tmp"))
	  (bash-completion-addsuffix "hello")))
      "hello/")

     ("bash-completion-addsuffix check directory, expand tilde"
      (flet ((file-accessible-directory-p (a) (equal a (concat (expand-file-name "y" "~/x")))))
	(let ((default-directory "~/x"))
	  (bash-completion-addsuffix "y")))
      "y/")

     ("bash-completion-starts-with"
      (list
       (bash-completion-starts-with "" "hello ")
       (bash-completion-starts-with "hello world" "hello ")
       (bash-completion-starts-with "hello world" "hullo ")
       (bash-completion-starts-with "hello" ""))
      '(nil t nil t))

     ("bash-completion-ends-with"
      (list
       (bash-completion-ends-with "" "world")
       (bash-completion-ends-with "hello world" "world")
       (bash-completion-ends-with "hello world" "wurld")
       (bash-completion-ends-with "hello" ""))
      '(nil t nil t))

     ("bash-completion-last-wordbreak-split"
      (list
       (bash-completion-last-wordbreak-split "a:b:c:d:e")
       (bash-completion-last-wordbreak-split "hello=world")
       (bash-completion-last-wordbreak-split "hello>world")
       (bash-completion-last-wordbreak-split "hello"))
      '(("a:b:c:d:" . "e")
	("hello=" . "world")
	("hello>" . "world")
	("" . "hello")))


     ("bash-completion-before-last-wordbreak"
      (list
       (bash-completion-before-last-wordbreak "a:b:c:d:e")
       (bash-completion-before-last-wordbreak "hello=world")
       (bash-completion-before-last-wordbreak "hello>world")
       (bash-completion-before-last-wordbreak "hello"))
      '("a:b:c:d:" "hello=" "hello>" ""))

     ("bash-completion-after-last-wordbreak"
      (list
       (bash-completion-after-last-wordbreak "a:b:c:d:e")
       (bash-completion-after-last-wordbreak "hello=world")
       (bash-completion-after-last-wordbreak "hello>world")
       (bash-completion-after-last-wordbreak "hello"))
      '("e" "world" "world" "hello"))

     ("bash-completion-fix escape rest"
      (bash-completion-fix "a\\ bc d e" "a\\ b")
      "a\\ bc\\ d\\ e")

     ("bash-completion-fix do not escape final space"
      (bash-completion-fix "ab " "a")
      "ab ")

     ("bash-completion-fix unexpand home and escape"
      (bash-completion-fix (expand-file-name "~/a/hello world") "~/a/he")
      "~/a/hello\\ world")

     ("bash-completion-fix match after wordbreak and escape"
      (bash-completion-fix "hello world" "a:b:c:he")
      "a:b:c:hello\\ world")

     ("bash-completion-fix just append"
      (bash-completion-fix " world" "hello")
      "hello\\ world")

     ("bash-completion-extract"
      (flet ((bash-completion-buffer () (current-buffer)))
	(sz-testutils-with-buffer
	 "hello world\nhello \n\n"
	 (bash-completion-extract "hello")))
      '("hello\\ world" "hello "))

     ("bash-completion-nonsep"
      (list
       (bash-completion-nonsep nil)
       (bash-completion-nonsep ?')
       (bash-completion-nonsep ?\"))
      '("^ \t\n\r'\"" "^ \t\n\r'" "^ \t\n\r\""))


     ("bash-completion-escape"
      (bash-completion-escape "He said: \"hello, 'you'\"")
      "He\\ said:\\ \\\"hello,\\ \\'you\\'\\\"")

     ("bash-completion-escape not if double quoted"
      (bash-completion-escape "\"hello, you")
      "\"hello, you")

     ("bash-completion-escape not if single quoted"
      (bash-completion-escape "'hello, you")
      "'hello, you")

     ("bash-completion-quote allowed"
      (bash-completion-quote "abc_ABC/1-2.3")
      "abc_ABC/1-2.3")

     ("bash-completion-quote quoted"
      (bash-completion-quote "a$b")
      "'a$b'")

     ("bash-completion-quote quoted single quote"
      (bash-completion-quote "a'b")
      "'a'\\''b'")

     ("bash-completion-join"
      (bash-completion-join '("ls" "-l" "/a/b" "/a/b c" "/a/b'c" "$help/d"))
      "ls -l /a/b '/a/b c' '/a/b'\\''c' '$help/d'")

     ))

  ;; ---------- integration tests

  (defmacro bash-completion_test-harness (&rest body)
    `(let ((bash-completion-process nil) (bash-completion-alist nil))
      (unwind-protect
	  (progn ,@body)
	;; tearDown
	(condition-case err
	    (when bash-completion-process
	      (let ((buffer (process-buffer bash-completion-process)))
		(kill-process bash-completion-process)
		(kill-buffer buffer)))
	  (error (message "error in bash-completion_test tearDown: %s" err))))))

  (defmacro bash-completion_test-with-shell (&rest body)
    `(bash-completion_test-harness
	(let ((shell-buffer nil)
	      (explicit-shell-file-name bash-completion-prog))
	  (unwind-protect
	      (progn
		(setq shell-buffer (shell (generate-new-buffer-name "*bash-completion_test-with-shell*")))
		;; accept process output until there's nothing left
		(while (accept-process-output nil 0.6))
		;; do a completion and return the result
		(with-current-buffer shell-buffer
		  (let ((start (point)))
		    (progn ,@body)
		    (buffer-substring-no-properties start (point-max)))))
	    ;; finally
	    (when (and shell-buffer (buffer-live-p shell-buffer))
	      (with-current-buffer shell-buffer
		(insert "\nexit\n"))
	      (kill-buffer shell-buffer))))))

  (put 'bash-completion-regress-integration 'regression-suite t)
  (setq bash-completion-regress-integration '(
       ("bash-completion interaction"
	(bash-completion_test-harness
	 (list
	  (bash-completion-is-running)
	  (buffer-live-p (bash-completion-buffer))
	  (bash-completion-is-running)
	  (bash-completion-comm "hel" 4 '("hel") 0)
	  (progn
	    (bash-completion-send "echo $EMACS_BASH_COMPLETE")
	    (with-current-buffer (bash-completion-buffer)
	      (buffer-string)))
	  (bash-completion-reset)
	  (bash-completion-is-running)))
	'(nil t t ("help ") "t\n" nil nil))

       ("bash-completion setenv"
	(bash-completion_test-harness
	 (bash-completion-send "echo $EMACS_BASH_COMPLETE")
	 (with-current-buffer (bash-completion-buffer)
	   (buffer-string)))
	"t\n")

       ("bash-completion execute one completion"
	(bash-completion_test-with-shell
	 (let ((start (point)))
	   (insert "__bash_complete_")
	   (bash-completion-dynamic-complete)))
	"__bash_complete_wrapper ")

       )))

;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (when (featurep 'regress)
    (regress bash-completion-regress)
    (when bash-completion-run-integration-tests
      (regress bash-completion-regress-integration))))

;;; bash-completion_test.el ends here
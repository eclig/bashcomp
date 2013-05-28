This library defines dynamic completion hooks for Emacs `shell-mode'
and shell-command prompts that are based on Bash's native completion.

You will need shell-command.el to get tab completion in the
minibuffer. See [http://www.namazu.org/~tsuchiya/elisp/shell-command.el](http://www.namazu.org/~tsuchiya/elisp/shell-command.el)

Bash completion for Emacs:

- is aware of Bash builtins, aliases and functions.
- does file expansion inside of colon-separated variables
  and after redirections (> or <).
- escapes special characters when expanding file names.
- is configurable through Bash's programmable completion.

A simpler and more complete alternative to bash-completion.el is to
run a bash shell in a buffer in term mode(M-x `ansi-term').
Unfortunately, many Emacs editing features are not available when
running in term mode.  Also, term mode is not available in
shell-command prompts.

## INSTALLATION

1. copy bash-completion.el into a directory that's on Emacs load-path
2. add this into your .emacs file:

        (autoload 'bash-completion-dynamic-complete 
          "bash-completion"
          "BASH completion hook")
        (add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
        (add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)

  or simpler, but forces you to load this file at startup:

        (require 'bash-completion)
        (bash-completion-setup)

3. reload your .emacs (M-x `eval-buffer') or restart

4. Set `comint-prompt-regexp' to match your Bash prompt, regardless
   of the value of `comint-use-prompt-regexp'.  This is needed by
   `comint-redirect', the library used for getting the completions
   from the underlying shell process.

Once this is done, use <TAB> as usual to do dynamic completion from
shell mode or a shell command minibuffer, such as the one started
for M-x `compile'.

You'll get better results if you turn Bash's programmable completion on.
On Ubuntu, this means running:

    sudo apt-get install bash-completion

and then adding this to your ~/.bashrc:

    . /etc/bash_completion

Right after enabling Bash's programmable completion, and whenever you
make changes to you ~/.bashrc, call `bash-completion-reset' to make
sure bash completion takes your new settings into account.

## CAVEATS

Using the underlying Shell process for doing the completion has some
important disadvantages:
- Bash completion is slower than standard Emacs completion.
- the first completion can take a bit longer, since a table of
  available Bash completions needs to be initialized.
- The variable `comint-prompt-regexp' hast be set to the
  correct prompt for your Shell..

## COMPATIBILITY

bash-completion.el is quite sensitive to the OS and BASH version.
This package is known to work on the following environment:

- GNU Emacs 22.3.1 (Aquamacs 1.7)
- GNU Emacs 22.1.1 (OSX 10.5)
- GNU Emacs 22.1.1 (Ubuntu 8.04)
- GNU Emacs 23.0.94.1 (Ubuntu 8.10)
- GNU Emacs 24.1.1 (OSX 10.7)
- GNU Emacs 24.1.1 (OSX 10.8)
- GNU Emacs 24.3

and using the following bash versions:

- Bash 2.05.08
- Bash 3.2.17
- Bash 3.2.32
- Bash 3.2.39

bash-completion.el does not works on XEmacs.

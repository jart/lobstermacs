
# Lobstermacs Manual

Lobstermacs is distribution of emacs that sets up all the latest and
greatest features for you automatically.  Lobstermacs is intended to
*just work* with batteries included.

Lobstermacs also makes emacs simpler by improving user interface
consistency across different "modes" or programming languages.
Lobstermacs also provides an easy to understand manual that makes it
easy to get started and offers real-world advise on how to tackle
typical workflows.

Lobstermacs is intended for the following audiences:

1.  Career Programmers accustomed to IDEs such as Visual Studio,
    Eclipse, TextMate, etc. who are interested in devoting the time
    and effort to learn a more robust editor.

2.  Emacs veterans who'd like a sane starting place for their own
    configurations, to gain exposure to bleeding-edge features, or to
    simply expose themselves to new perspectives on editing.

3.  Computer power-users and hobbyists who want to have more fun
    editing text.

## The Most Important Things to Know

ToDo

## Basic Editing

ToDo

## Using Emacs from the Command Line

Running in terminal mode:

    # mnemonic for -nw: "no window"
    emacs -nw

Emacs takes a while to load.  To quickly edit a file from the command
line, you can start emacs in the background and use `emacsclient` to
quickly edit a file:

    emacs &
    emacsclient -nw some_file.txt
    emacsclient -nw some_file.txt --alternate-editor=nano

Running lisp code from the command line:

    emacsclient -e '(+ 2 2)'
    emacsclient -e <code_file.el

## Using Emacs for System Administration

Getting started:

    jart@compy:~$ ssh myserver
    jart@myserver:~$ sudo apt-get install emacs-snapshot-nox emacs-snapshot-el
    jart@myserver:~$ git clone git://github.com/jart/emacs-starter-kit.git .emacs.d
    jart@myserver:~$ emacs

Important Shortcuts:

* `F3`: Re-open current file with root permissions using sudo.
* `M-x sudo-edit`: Open a new file with root permissions.

### BIND DNS Zone Files

When editing DNS zone files, `zone-mode` saves you time by setting the
zone serial number to the current date each time you save your file.
This mode is automatically loaded when editing files with naming
schemes like: `something.com.db`, `bind/db.something` or
`named/db.something`.

## Lisp

ToDo

### Paredit: Dealing With Parentheses (S-Expressions)

Paredit takes a little time to get used to but is very much worth
learning if you hack Lisp.  Paredit works by overriding the behavior
of keys make it *next to impossible* to have unbalanced parentheses in
your Lisp.

Paredit works by overriding the bindings for things like `(`, `[`,
etc.  You can press `(` over a highlighted region.  Destructive
commands like delete, `C-k`, etc. are overridden to act "smarter"
inside parentheses.

#### Important Shortcuts

* `C-M-f` and `C-M-b`: Hop around your jungle of s-exprs (or `C-F` and `C-B`)
* `M-s`: Slurp next term and put it inside my s-expr
* `M-v`: Vomit last term in current s-expr
* `M-r`: Bust me out of the s-expr I'm wrapped inside
* `M-S`, `M-J`: Split/Join an s-expr

#### Advanced

* `C-)`, `C-(`: If your s-expr needs more friends (slurping)
* `C-}`, `C-{`: To kick stuff out of your s-expr (barfing)

#### Customize

    (eval-after-load 'paredit
      '(progn
         (define-key paredit-mode-map (kbd ")")   'paredit-close-parenthesis)
         (define-key paredit-mode-map (kbd "M-)") 'paredit-close-parenthesis-and-newline)
         (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
         (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)))

## Python

ToDo

## Ruby

ToDo

## Erlang

You may want to edit `~/.erlang` so auto-completion will work:

    code:add_pathsz(["/home/jart/code/switchboard/ebin",
                     "/home/jart/code/switchboard/deps/mochiweb/ebin",
                     "/home/jart/code/switchboard/deps/ibrowse/ebin"]).

As soon as you start editing erlang code, an interactive Erlang VM
shell will be started named `emacs@<host>` using buffer name
`*erlang*`.

### Important Shortcuts

* `M-?`: Show completions for current function or module name
* `M-.`: Go to symbol definition
* `C-c C-d :`: Evaluate an erlang expression (similar to `M-!`)
* `C-c C-k`: Compile current buffer and load into Erlang VM

Remote Nodes:

* `C-c C-d n`: Connect to a specific erlang mode.
* `C-c C-d l`: List erlang processes.

Less Essential:

* `C-c C-d F`: Open source code for given module name.

### Example #1: Getting Started

Open up a file named `lol.erl` and enter the following:

    -module(lol).
    -export([hello_world/0]).

    hello_world() ->
        "hello kitty".

Now compile and load your "lol" module by typing `C-c C-k`.  You
should see the following:

    (emacs@orange)4> c("/home/jart/lol", [{outdir, "/home/jart/"}]).
    {ok,lol}

Now try out your new function!

    (emacs@orange)5> lol:hello_world().
    "hello kitty"

### Example #2: Make a Web Application with Mochiweb

## Restructured Text

Important Shortcuts:

* `C-c 4`: Preview current file as PDF (typesetter: rst2pdf)
* `C-c 5`: Preview current file as PDF (typesetter: LaTeX)
* `C-c 6`: Preview current file as HTML in your browser
* `C-=`: Turn current line into a section header and/or make sure the
  bar of `=` or `-` characters is the correct length.  Press multiple
  times to cycle through different types of section headers.

## About

    Lobstermacs
    Based upon the Emacs Starter Kit Project
    Licensed under the GPL v3 or later

    J.A. Roberts Tunney <jtunney@lobstertech.com>

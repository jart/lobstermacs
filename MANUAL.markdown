
# Lobstermacs Manual

Lobstermacs: A Happier Version of Emacs

* Maintainer: J.A. Roberts Tunney <jtunney@lobstertech.com>
* Website: <http://github.com/jart/lobstermacs/>
* Copying: Licensed under the GNU GPL v3 or later
* Credits: This is an upstream compatible "friendly fork" of all
  terrific work done by everyone on GitHub with
  [emacs-starter-kit][http://github.com/technomancy/emacs-starter-kit].

## Synopsis

Lobstermacs is distribution of emacs, much in the way that Ubuntu is a
distribution of Linux.  It's a collection of Emacs' best third party
extensions, fine-tuned to be simpler and less annoying combined with
helpful documentation.

## Why On Earth Would I Want To Use Your Goofy Flavor of Emacs?

* Works great on Linux/Macintosh/Windows as well as the terminal.
  Ubuntu is the platform the developers choose.

* ido support (best feature ever omg) Opening files, switching between
  hundreds of open files, browsing through functions/classes
  definitions, etc. is much more efficient and user-friendly than the
  default behavior.

* Code intelligence, completion, definition jumping and browsing work
  out of the box for Lisp, Ruby, Python, Erlang and C/C++.  We try to
  use company-mode drop-down completion boxes whenever possible.

* nXhtml support is built in.  This means you can edit a PHP file with
  inlined HTML (if you really wanted to lol) as well as many types of
  HTML templating languages like Smarty, Django, etc. without
  sacrificing XHTML validation/awareness.

* magit support for *fantastic* Git support.

* paredit support which makes editing Lisp languages less painful.

* The shell command feature (`M-!`) has tab completion.  The up/down
  as well as `C-n`/`C-p` will let you cycle through your command
  history.

* You can press `F3` to re-open a file with sudo permissions.

* Python and Lisp will transform 'lambda' into a pretty symbol.

* Lobstermacs' key-bindings have been tuned to be more accessible to
  Windows/Mac users without compromising the standard bindings. (Work
  In Progress)

* Uses crazy heuristics to automatically determine the location of the
  project you're currently editing, how it should be
  compiled/executed, etc.

* Recursive grep is now more user-friendly.  Searches current folder
  or entire project if base folder could be determined.  Ignores
  certain types of "evil" files like `.svn` folders as well as what's
  in your `.hgignore` or `.gitignore` (if present.)

* Excellent support for Python projects that are using virtualenv.

* Simplified PDF/browser preview support for Restructured Text and
  Markdown.  You can render documents using HTML, LaTeX, man, and
  more.

* Ediff doesn't put that annoying little frame in the top right corner
  of your screen anymore.

* YASnippet works out of the box for more than a dozen languages.  If
  you're a fan of TextMate, you should feel right at home with
  Lobstermacs <3

## Reporting Bugs

You can report an issue to the [Lobstermacs Issue
Tracker]<http://github.com/jart/lobstermacs/issues>, or contact me
directly:

* Email: <jtunney@lobstertech.com>
* SIP: <sip:jtunney@lobstertech.com>
* XMPP: <xmpp:jtunney@gmail.com>

## Installation

### Ubuntu (Strongly Recommended)

This will get you set up with a nice happy GUI version of emacs.  (If
you want the non-GUI version for a dedicated server, use
`emacs-snapshot-nox`)

    $ sudo apt-get install git-core emacs-snapshot emacs-snapshot-el ispell
    $ git clone git://github.com/jart/lobstermacs.git
    $ emacs-snapshot --debug-init -q -l lobstermacs/init.el

Run Lobstermacs in a terminal.  (If you are using a terminal, I
*strongly* recommend trying out GNU Screen.  See
`lobstermacs/contrib/.screen` for helpful information.)

    $ export TERM=xterm-256color
    $ emacs-snapshot --debug-init -q -l lobstermacs/init.el

## Basic Editing

The best way to learn basic editing using the keyboard is to go
through the built-in tutorial under `Help | Emacs Tutorial` or by
pressing `C-h t`.

## Universal Shortcuts

These keyboard shortcuts have been tuned to work consistently across a
variety of editing modes.

### Essential

Note: A "buffer" is an open file.  Most Emacs users keep hundreds open
at a time.

* `C-g`: Panic button (makes whatever you did stop happening)
* `C-x C-c`: Quit
* `M-z` or `C-?`: Undo!
* `C-x 1`: Make split windows go away
* `C-x C-f`: Open file (Tip: Try `//`, `C-s` and `C-r` in IDO's find file mode)
* `C-x f`: Open recent file
* `M-s` or `C-x C-s`: Save current buffer
* `C-x k`: Close buffer
* `C-x C-b`: Switch between buffers IDO style (Tip: `C-s` and `C-r`
  cycle through your available buffers)
* `C-<middle-click>`: Shows pop-up menu of open buffers, grouped by language mode
* `M-x` or `C-x C-m`: Execute command manually (IDO style!)
* `M-!`: Execute system/shell command

### Function Keys

* `F1`: Search Google
* `F2`: Open man page
* `F3`: Re-open current buffer with sudo permissions (or `M-x sudo-edit`)
* `F4`: Toggle directory browser tray on side using sr-speedbar.
  Supports revision control and stuff.
* `F9` or `C-x m`: Open up a shell or command prompt (or switch to
  active one.  `C-x M` creates additional shells.
* `F10`: Compile project
* `F11` or `C-x C-n`: Jump to next compilation error
* `F12` or `C-x C-p`: Jump to previous compilation error

### Informative

* `C-h k`: Tells you what a keyboard shortcut actually does.
* `C-h b`: Show a list of all available keyboard shortcuts.  This list
  will look slightly different depending on what type of file you're
  editing.

### Searching

Once you start searching, you can press `C-s` and `C-r` to jump
between results, and `C-g` will cancel and bring you back where you
were.

* `C-s`: Search forward (Uses regular expressions)
* `C-r`: Reverse search (Uses regular expressions)
* `C-x C-r`: Replace string in current buffer or region [1]
* `C-x C-l`: Replace string (regexp) in current buffer or region [1]
* `C-%`: Replace string, but ask me to confirm each replacement
* `M-g g`: Goto line

* `C-x C-g`: Search for text in current project or folder and show me
  a list of occurrences lines (grep) [1]
* `F11` or `C-x C-n`: Jump to next grep result
* `F12` or `C-x C-p`: Jump to previous grep result

### Modifying Text

* `M-$`: Spell check word
* `M-c`: Capitalize word
* `M-u`: Uppercase word
* `M-l`: Lowercase word
* `C-t`: Transpose characters
* `M-t`: Transpose words
* `C-M-t` or `C-x t`: Transpose lines
* `M-q`: Fill paragraph under cursor.  This wraps lines at column 70
  or so to give you neat little blocks of text.
* `M-Q`: Un-fill paragraph under cursor.  (Reverses `M-q`)

Any shortcuts that deletes more than one character ALSO copies the
text you just deleted (to the "kill ring.")  You can yank (or paste)
it back with `C-y`.

* `C-z` or `M-z` or `C-?`: Undo! (`C-z` disabled in terminal mode)
* `C-<Backspace>`: Delete word backwards
* `C-M-h`: Delete word backwards
* `C-d`: Delete character forwards
* `M-d`: Delete word forward
* `C-k`: Delete (Kill) current line

### Copy/Pasting

Emacs is like, *really old* so instead of a clip-board you get a "kill
ring."  Kill=Copy, Yank=Paste, Region=Highlighted Text.

You can highlight text with shift + arrow keys if you like.

* `C-y` or `Shift-Insert` or `Middle-Click`: Yank (Pastes last thing killed)
* `C-y M-y M-y M-y ...`: Yank, cycling through previous kills
* `C-<space>`: Start highlighting text (mark region)
* `C-w`: Cut region to clipboard
* `M-w`: Copy region to clipboard
* `C-x h`: Highlight entire buffer (Select all)
* `C-g`: Cancel highlighting text
  
Tip: Many common features are coded to operate on highlighted regions.

Tip: It helps to use other shortcuts like Search (`C-s`) once you're
started highlighting.

### Cursor Movement

How to code like a hacker.  You are recommended to avoid the arrow
keys and do things the Emacs way for sake of efficiency, but the
choice is yours!

* `C-l`: Re-center screen (press multiple times for different alignments)

* `C-f` or `<right>`: *Forward* one character
* `C-b` or `<left>`: *Backward* one character
* `M-f` or `<C-right>`: *Forward* one word
* `M-b` or `<C-left>`: *Backward* one word
* `C-M-f`: Next *term* (May conflict with OS or Term)
* `C-M-b`: Previous *term* [1] [2] (May conflict with OS or Term)

* `C-n` or `<down>`: *Next* line
* `C-p` or `<up>`: *Previous* line
* `M-n` or `C-<down>`: Next *blank* line (think jump to next paragraph) [1]
* `M-p` or `C-<up>`: Previous *blank* line [1]
* `C-M-n` or `C-{`: Next *top-level term* [1] [2]
* `C-M-p` or `C-}`: Previous *top-level term* [1] [2]

* `C-a`: Go to beginning of line (Think first letter in alphabet)
* `C-e`: Go to *End* of line
* `C-v` or `<PgUp>`: Page up
* `M-v` or `<PgDown>`: Page down
* `M->` or `C-<home>`: Go to end of buffer
* `M-<` or `C-<end>`: Go to beginning of buffer 
* `C-<PgUp>`: Move cursor to top of screen without scrolling
* `C-<PgDown>`: Move cursor to bottom of screen without scrolling

### Programming Shortcuts

* `<Tab>`: Fix indentation for current line

* `Highlight + <Tab>`: Fix indentation on multiple lines

* `C-x C-i`: IDO-style browsing of functions/class definitions in
  current buffer and (ToDo) other currently open files written in the
  same programming language.  Does not require a TAGS file.

* `M-/`: Auto-completes a partially typed word based on other words in
  your buffer, file-names, function/class symbols for your programming
  project, as well as words from the dictionary.  Press multiple times
  to cycle through possible completions.

* `M-.`: Jump to definition of symbol under cursor.

* `C-;`: Comment highlighted region

* `C-o`: Create new line without indentation.  This is very useful in
  cases where you need a blank line but pressing `<enter>` would do
  something undesirable.  (Like with auto-fill and auto-indent)

### Programming Tools

* `M-x delete-trailing-whitespace`: Does what it says on whole buffer

### Macros (Advanced)

Macros are very useful for automating basic editing tasks, and they're
usually simpler than writing a regular expression.

* `C-(`: Start recording macro (For )
* `C-)`: Stop recording and save macro
* `C-x e`: Execute last saved macro (Saves current macro if making one)
* `C-g`: Cancel recording macro

### Window Splitting (Advanced)

"Window" is a sort of a misnomer.  In the emacs world, this means a
frame to show multiple buffers within a single Emacs screen.  ("Frame"
is the term to describe real windows)

* `C-x 3`: Split to right-hand side
* `C-x 2`: Split below
* `C-x C-t` or `C-x o`: Jump from window to window
* `C-x 1`: Close *all* other windows except one I'm inside
* `C-x 0`: Close only current window, leaving the rest open

[1] Lobstermacs only
[2] Intended more for programmers

### Narrowing (Advanced)

This is a goofy feature.

C-u n d         narrow to function
C-u n n         narrow to region
C-u n p         narrow to page
C-u n w         widen

## Customizing

Once you save your customizations, they will be written to
`custom.el`.

* `M-x customize-apropos`: Useful for searching 

### Colors

* `M-x customize-face`: A long list of colors/fonts you can customize
  throughout Emacs.  After pressing "Save for future sessions" check
  out your `custom.el` file to see what it did.

* `M-x list-colors-display`: Shows you a list of all neat colorful
  list of all the different text foreground/background colors you can
  use.

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

### BIND DNS Zone Files

When editing DNS zone files, `zone-mode` saves you time by setting the
zone serial number to the current date each time you save your file.
This mode is automatically loaded when editing files with naming
schemes like: `something.com.db`, `bind/db.something` or
`named/db.something`.

## Lisp

### Emacs Lisp

* `C-x C-e`: Execute s-expression before cursor or region
* `C-:`: Ask me to type a s-expression in minibuffer to execute

Tip: Use EDebug to diagnose Lisp problems.  I find `M-x edebug-defun`
is very useful!

### Paredit: Dealing With Parentheses (S-Expressions)

Paredit takes a little time to get used to but is very much worth
learning if you hack Lisp.  Paredit works by overriding the behavior
of keys make it *next to impossible* to have unbalanced parentheses in
your Lisp.

Paredit works by overriding the bindings for things like `(`, `[`,
etc.  You can press `(` over a highlighted region.  Destructive
commands like delete, `C-k`, etc. are overridden to act "smarter"
inside parentheses.

Important Shortcuts:

* `C-M-f` and `C-M-b`: Hop around your jungle of s-exprs (or `C-F` and `C-B`)
* `C-c C-s`: Slurp next term and put it inside my s-expr
* `C-c C-b`: Barf last term in current s-expr out of the club
* `C-c C-r`: Raise me out of the s-expr I'm trapped inside
* `C-c C-l`: Split apart an s-expr or string at cursor
* `C-c C-j`: Join two s-exprs or strings at left and right of cursor

Advanced:

* `C-)`, `C-(`: If your s-expr needs more friends (slurping)
* `C-}`, `C-{`: To kick stuff out of your s-expr (barfing)

## Python

Ubuntu users starting off with Python should install the following
packages to get started:

    sudo apt-get install python python-setuptools python-tz
    sudo easy_install django # 9.04 ships with 1.0.. 1.1 is so worth it!
    sudo easy_install py     # my personal favorite unit testing framework

## Ruby

Ubuntu users starting off with Ruby and/or Rails should install the
following packages:

    sudo apt-get install ri ruby irb rails mongrel rake rdoc rubygems

## Erlang

The first time you edit an Erlang source code file, Lobstermacs will
automatically:

1. Attempt to compile the Distel extension and Lobstermacs support
   libraries (ToDo)

2. An interactive Erlang VM shell will be started named `emacs@<host>`
   using buffer name `*erlang*`.  This is used for the
   code-intelligence features.

3. Add your project's `ebin` and `deps/*` folders to
   `code:add_pathsz()`.  If they can't be found, adds current
   directory to Erlang path.

4. Activate an auto-reload module.  This means every time your press
   `F9` to compile, your Erlang modules will instantly be re-loaded
   into memory.

Note: These helpful features only apply to the `emacs@<host>` session.
If you open up an Erlang shell to a remote production server, you can
rest assured that Lobstermacs will be nice :)

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

## Revision Control

Revision control is a *invaluable tool* if you're a programmer.
Lobstermacs has excellent VCS support that you're going to love:

### Git using Magit

When working on GitHub projects, or hacking the Linux kernel,
Lobstermacs offers amazing Git support using "Magit."  The best thing
about Magit is that it turns Git's concept of a "Staging Area" from an
annoyance to an indispensable tool to avoid making humongous commits.

Note: If you have never used Git before, pick a folder with lots of
code and run `git init`, `git add .` then `git commit -a -m 'Initial
Checkin'`.  Now change a bunch of files and you're ready to get
started:

* `C-x g`: Open up Git status buffer

Status Window Shortcuts:

* `g`: Refresh the Git status buffer with the latest information
* `c`: Start a commit
* `P`: Push your latest commits to a server like GitHub
* `F`: Pull new commits other people have made from the server
* `k`: Discard changes made to a file (like `svn revert`)
* `i`: Add file to your `.gitignore` file

Staging Files and Hunks:

* `<tab>`: Toggles expansion of hunks (see what changed in a file, and
  cherry-pick only specific hunks that you want to commit)
* `n` and `p`: Jump between hunks
* `s`: Stage file or hunk to be committed
* `u`: Stage file or hunk to be committed
* `<enter>`: Jump to the source code for this file or hunk

See the menu-bar named 'Magit' which appears when you open the status
window for more information.  If you don't have a menu bar, type `C-h
m` for more information.

For more information: <http://zagadka.vm.bytemark.co.uk/magit/magit.html>

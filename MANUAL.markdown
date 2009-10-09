
# Lobstermacs Manual

Lobstermacs: Emacs For the 21st Century

<http://github.com/jart/lobstermacs/>

## Synopsis

Lobstermacs is distribution of emacs, much in the way that Ubuntu is a
distribution of Linux.  It's a collection of Emacs' best third party
extensions, fine-tuned for consistency and simplicity with helpful
documentation.

## Rationale

Emacs is an excellent editor, but rather "vanilla" out-of-the-box.
It's been around for decades, has thousands of configuration options,
modes and third-party libraries.  Documentation and on-line resources
are written in a language few programmers understand, and are
oftentimes incredibly buggy or outdated.

Lobstermacs offers a simpler alternative.  It works out of the box
with *all batteries included*, hand-selecting the best features the
Emacs community has to offer, making them simpler to use, and offering
concise documentation to get you started.

I created Lobstermacs because I noticed that most of my friends who
used Emacs didn't have the time and patience to deal with its quirks,
and became so discouraged by Emacs' complexity that they never
bothered to learn all of its coolest features.

Lobstermacs is not a watered-down or intended for newbies, but rather
to promote efficiency for professional engineers without compromising
sanity.  Compared to software like Dreamweaver and Visual Basic,
Lobstermacs still has a strong learning curve.  Whether or not the
learning curve is worth it is for you to decide.

## Why On Earth Would I Want To Use Your Goofy Flavor of Emacs?

* Antiquated GUI features are disabled.  Copy and pasting will also
  work as you'd expect between applications.  Right clicking is
  favored over middle clicking.

* Switching between buffers and opening files is much more
  user-friendly and insanely efficient thanks to IDO.

* Code completion and definition jumping work out of the box for Lisp,
  Ruby, Python and Erlang.

* Works great on Linux/MacOSX/Windows as well as the terminal.
  Top-notch support is offered for Ubuntu users.

* You can press `F3` to re-open a file with sudo permissions.

* The shell command feature (`M-!`) has tab completion and the up/down
  keys will let you browse through your command history.

* Simplified PDF/browser preview support for Restructured Text and
  Markdown.

* Uses extensive heuristics to automatically determine the location of
  the project you're currently editing, how it should be
  compiled/executed, etc.

* Recursive grep is now more user-friendly.  Searches current folder
  or entire project if base folder could be determined.  Ignores
  certain types of "evil" files like `.svn` folders as well as what's
  in your `.hgignore` or `.gitignore` (if present.)

* Top-notch git version control support thanks to magit.  Also
  supports Mercurial, Subversion, CVS, and more.

* Excellent support for Python projects that are using virtualenv.

* Python and Lisp will transform 'lambda' into a pretty symbol.

* Lisp s-expressions can be edited more easily thanks to paredit.

* Ediff doesn't put that annoying little frame in the top right corner
  of your screen anymore.

## Reporting Bugs

Lobstermacs is a small project so all suggestions and bug reports are
welcome; no matter how trivial they may be.  *Please, please, please,*
if you find **ANYTHING** annoying, difficult, broken, or have a
difference in opinion, I urge you to complain!  You can use the
following:

1. Issue tracker: <http://github.com/jart/lobstermacs/issues>

2. Emacs me directly: <jtunney@lobstertech.com>

3. Chat with me on Google Talk for support: <jtunney@gmail.com>

Even if you get some sort of crash and are experienced enough to
figure it out what went wrong, please report it so it doesn't happen
to anyone else!

## Installation

### Ubuntu (Strongly Recommended)

This will get you set up with a nice happy GUI version of emacs.  (If
you want the non-GUI version for a dedicated server, use
`emacs-snapshot-nox`)

    $ sudo apt-get install git-core emacs-snapshot emacs-snapshot-el ispell
    $ git clone git://github.com/jart/emacs-starter-kit.git
    $ emacs-snapshot --debug-init -q -l emacs-starter-kit/init.el

## Basic Editing

The best way to learn basic editing using the keyboard is to go
through the built-in tutorial under `Help | Emacs Tutorial` or by
pressing `C-h t`.

## Universal Shortcuts

These shortcuts apply to all modes.  Many are Lobstermacs specific and
have been tuned to work consistently across modes.  Many of these
shortcuts, particularly the navigational ones, are supported on almost
all terminals and throughout Mac OS X (by default.)

### Essential

* `C-g`: Panic button (makes whatever you did stop happening)
* `C-x C-c`: Quit
* `M-z` or `C-?`: Undo!
* `C-x 1`: Make split windows go away
* `C-x C-f`: Open file (creating a new buffer)
* `C-x f`: Open recent file
* `M-s` or `C-x C-s`: Save current buffer
* `C-x k`: Close buffer
* `C-x C-b`: Switch between buffers (as in open files)
* `M-x`: Execute extended command
* `M-!`: Execute system/shell command

### Function Keys

* `F1`: Search Google
* `F2`: Open man page
* `F3`: Re-open current buffer with sudo permissions (or `M-x sudo-edit`)
* `F9`: Open a command prompt
* `F10`: Compile project
* `F11` or `C-x C-n`: Jump to next compilation error
* `F12` or `C-x C-p`: Jump to previous compilation error

### Informative

* `C-h k`: Tells you what a keyboard shortcut actually does.
* `C-h b`: Show a list of all available keyboard shortcuts.  This list
  will look slightly different depending on what type of file you're
  editing.

### Searching

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

Any shortcuts that deletes more than one character ALSO copies the
text you just deleted (to the "kill ring.")  You can yank (or paste)
it back with `C-y`.

* `M-z` or `C-?`: Undo!
* `C-<Backspace>`: Delete word backwards
* `C-M-h`: Delete word backwards
* `C-d`: Delete character forwards
* `M-d`: Delete word forward
* `C-k`: Delete (Kill) current line

### Code Intelligence

* `M-/`: Expand symbol based on text around it (dumb, works universally)
* `M-?` or `C-<enter>`: Auto-complete symbol (smart, introspects code)
* `C-;`: Comment highlighted region [2]

### Cursor Movement (Advanced If You're Lazy)

* `C-l`: Re-center screen (press multiple times for different alignments)

* `C-f`: *Forward* one character
* `C-b`: *Backward* one character
* `M-f`: *Forward* one word
* `M-b`: *Backward* one word
* `C-M-f`: Next *term* [1] [2]
* `C-M-b`: Previous *term* [1] [2]

* `C-n`: *Next* line
* `C-p`: *Previous* line
* `M-n`: Next *blank* line [1]
* `M-p`: Previous *blank* line [1]
* `C-M-n`: Next *top-level term* [1] [2]
* `C-M-p`: Previous *top-level term* [1] [2]

* `C-a`: Go to beginning of line (Think first letter in alphabet)
* `C-e`: Go to *End* of line
* `C-v`: Page up
* `M-v`: Page down

### Copy/Pasting (Advanced)

AKA Killing/Yanking

You can highlight text with the keyboard (regions.)  Many shortcuts
detect regions.  If you highlight text and use replace string, it will
only work inside your region.

Tip: It helps to use other shortcuts like Search (`C-s`) once you're
started highlighting.

* `C-y`: Paste (Yank)
* `C-y M-y M-y M-y ...`: Paste something previously copied
* `C-<space>`: Start highlighting text (mark region)
* `C-w`: Cut region to clipboard
* `M-w`: Copy region to clipboard

### Macros (Advanced)

* `C-o`: Create new line without indentation (Very useful for creating
  blank lines in code when auto-indent is enabled)

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

## About

    Lobstermacs
    Based upon the Emacs Starter Kit Project
    Licensed under the GPL v3 or later

    J.A. Roberts Tunney <jtunney@lobstertech.com>

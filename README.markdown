# Lobstermacs

A friendly fork of Emacs Starter Kit.

![ScreenShot](http://lobstertech.com/media/misc/starter-kit.png)

Lobstermacs is intended for professional computer programmers.  This
is pre-alpha software and a hobby project with one developer so let
the user beware.


## Eventual Goal

- Fine-tune the latest/greatest features to *just work* without bugs.

- Python support...  Like *really amazing* Python support that's easy
  to use, requires no configuration and isn't ugly.


## What *Unique* Features Have Been Written So Far?

- Interactive Python programming.  `C-x C-e` and `C-c v` now work just
  as well for Python as they do for Emacs Lisp.  This is a more
  intuitive interface to Pymacs.  See `lob/python-eval`,
  `lob/python-eval-file` and `lob/python-eval-file-in-thread`.

- Erlang: Many steps have been taken so far to make the Distel
  extension *just work* for features like code completion.  (Still
  needs polishing)

- Restructured Text Previewing.  See `lob/rst-preview-html`,
  `lob/rst-preview-pdf` and `lob/rst-preview-pdf-latex`.  Emacs seems
  to have major issues viewing large PDF files.

- Made "zenburn" theme fix some of the crazy color schemes that ship
  with certain third-party libraries.

- `F3` will re-open current buffer with sudo permissions.


## Installation

- Ubuntu: `sudo apt-get install git-core emacs-snapshot emacs-snapshot-el ispell pymacs`
- Mac: [Download Emacs 23 for Mac OS X](http://emacsformacosx.com/)
- Windows: [Download Emacs 23 for Windows](http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.1-bin-i386.zip)

Initial Setup:

    git clone git://github.com/jart/lobstermacs.git ~/.emacs.d
    echo 'export TERM=xterm-256color' >>~/.bashrc
    source ~/.bashrc
    emacs-snapshot

Upgrade:

    cd ~/.emacs.d
    git pull


## Basic Usage

- Put your personal Lisp stuff: `~/.emacs.d/$USER.el`
- Emacs configuration settings are saved to: `~/.emacs.d/custom.el`
- ELPA Package Manager `M-x package-list-packages`


## License

- Lobstermacs is distributed under the same terms as Emacs itself (GPL
  v3+) See `COPYING` for more details.

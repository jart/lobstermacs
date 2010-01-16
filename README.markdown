# Lobstermacs

A friendly fork of Emacs Starter Kit.

![ScreenShot](http://lobstertech.com/media/misc/starter-kit.png)

Lobstermacs is intended for professional computer programmers.
Lobstermacs is one of my personal hobby projects.  This is pre-alpha
software so let the user beware.

The long goals are:

# Python support...  Really, really good Python support.
# Make Emacs friendlier to human beings.

What *unique* features have been written so far?

- Interactive Python programming.  `C-x C-e` and `C-c v` now work just
  as well for Python as they do for Emacs Lisp.  This is a more
  intuitive interface to Pymacs.  See `lob/python-eval`,
  `lob/python-eval-file` and `lob/python-eval-file-in-thread`.

- Erlang: Many steps have been taken so far to make the Distel
  extension *just work* for features like code completion.  (Still
  needs polishing)

- Restructured Text Previewing.  See `lob/rst-preview-html`,
  `lob/rst-preview-pdf` and `lob/rst-preview-pdf-latex`.

- Made "zenburn" color scheme fix some of the crazy color schemes that
  ship with certain third-party libraries.

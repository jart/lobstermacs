# Emacs Starter Kit

This should provide a saner set of defaults than you get normally with
Emacs. It's intended for beginners, but it should provide a reasonable
working environment for anyone using Emacs for dynamic languages. The
main advantage of the Starter Kit is that it provides better default
settings and bundles many useful libraries.

The latest version is at http://github.com/jart/emacs-starter-kit/

The latest "official" version is at http://github.com/technomancy/emacs-starter-kit/

## Notes Regarding This Fork

(jart 2009/08) I created this fork of emacs-starter-kit to write
documentation and offer better "out of the box" support for the
following pieces of software:

- Python
- Mercurial
- Erlang

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful. The
[Emacs Wiki](http://emacswiki.org) is also very handy.

### Tips

- You can press `F1` at any time to make the menu bar show up.

- If you picked *one feature* to learn, it should be "IDO"--one of
  emacs' most hyped features.  IDO lets you navigate and switch
  between a large number of files, very efficiently and intuitively.

- Type `M-x package-list-packages` to see cool ELPA packages you can
  install.

- Look at `starter-kit-bindings.el` to learn the keyboard shortcuts
  Emacs Starter Kit offers your over "vanilla" emacs.

- You can make emacs prettier by typing `M-x color-theme-zenburn`

## Installation

1. Install Emacs (at least version 22)
   Use your package manager if you have one.
   Otherwise, Mac users should get it [from Apple](http://www.apple.com/downloads/macosx/unix_open_source/carbonemacspackage.html).
   Windows users can get it [from GNU](http://ftp.gnu.org/gnu/emacs/windows/emacs-22.3-bin-i386.zip).
2. Move the directory containing this file to ~/.emacs.d [1]
   (If you already have a directory at ~/.emacs.d move it out of the way and put this there instead.)
3. Launch Emacs!

If you are missing some autoloads after an update (should manifest
itself as "void function: foobar" errors) try M-x regen-autoloads.

If you want to keep your regular ~/.emacs.d in place and just launch a
single instance using the starter kit, try the following invocation:

  $ emacs -q -l ~/src/emacs-starter-kit/init.el

Note that having a ~/.emacs file might override the starter kit
loading, so if you've having trouble loading it, make sure that file
is not present.

### Quick Start Ubuntu

This will get you set up with a nice happy GUI version of emacs:

    $ sudo apt-get install emacs-snapshot emacs-snapshot-el ispell \
                           js2-mode css-mode php-mode
    $ git clone git://github.com/jart/emacs-starter-kit.git
    $ emacs-snapshot --debug-init -q -l emacs-starter-kit/init.el

## Structure

The init.el file is where everything begins. It's the first file to
get loaded. The starter-kit-* files provide what I consider to be
better defaults, both for different programming languages and for
built-in Emacs features like bindings or registers.

Files that are pending submission to ELPA are bundled with the starter
kit under the directory elpa-to-submit/. The understanding is that
these are bundled just because nobody's gotten around to turning them
into packages, and the bundling of them is temporary. For these
libraries, autoloads will be generated and kept in the loaddefs.el
file. This allows them to be loaded on demand rather than at startup.

### Personal Customizations

Anything you customize using emacs' configuration GUI will be saved to
`config.el` which is ignored by git.

Some customizations don't belong in Emacs Starter Kit, and you may
only want them to load just for yourself, or load only on certain
computers.  Here's how to do it:

- User Specific: Make a file named `your_username.el` and it will be
  loaded automatically.  You could also make a folder named
  `your_username` if you have a bunch of `.el` files.

- Host Specific: Make a file named `hostname.el` or a folder named
  `hostname`.  If you're not sure what your host-name is, you can ask
  emacs by highlighting the following expression and pressing `C-x
  C-e`: (system-name)

## Emacs Lisp Package Archive

Libraries from [ELPA](http://tromey.com/elpa) are preferred when
available since dependencies are handled automatically, and the burden
to update them is removed from the user. In the long term, ideally
everything would be installed via ELPA, and only package.el would need
to be distributed with the starter kit. (Or better yet, package.el
would come with Emacs...) See starter-kit-elpa.el for a list of
libraries that are pending submission to ELPA. Packages get installed
in the elpa/ directory.

There's no vendor/ directory in the starter kit because if an external
library is useful enough to be bundled with the starter kit, it should
be useful enough to submit to ELPA so that everyone can use it, not
just users of the starter kit.

Sometimes packages are removed from the Starter Kit as they get added
to ELPA itself. This has occasionally caused problems with certain
packages. If you run into problems with such a package, try removing
everything from inside the elpa/ directory and invoking M-x
starter-kit-elpa-install in a fresh instance.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add
it. Take a look at what happens in init.el to get started.

Also: see the file TODO. Helping submit new libraries to ELPA is the
easiest way to help out. There are two ways you can do this: either
take new libraries and make them ready for ELPA, dropping them in the
elpa-to-submit directory or take files that are already in
elpa-to-submit, ensuring all their dependencies are correctly loaded
into ELPA, and sending them to the ELPA maintainer. There are details
at http://tromey.com/elpa/upload.html for how ELPA submission
works. Grep the project for TODO for other things.

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.

The latest version is at http://github.com/technomancy/emacs-starter-kit/

On Unix, /home/$USER/.emacs.d, on windows Documents and Settings/%your
user name%/Application Data

## Important: How To Not Hurt Your Wrists Using Emacs

People commonly choose editors like emacs and vi because they can edit
code more efficiently by keeping their fingers on "home row" rather
than constantly switching between the keyboard and mouse.

By default on modern keyboards, emacs kind-bindings are very painful
to type.  If you want to get the most out of emacs and make your
programming experience as happy as possible without getting carpal
tunnel syndrome, you should strongly consider doing the following:

1.  Swap the caps lock and ctrl key!!!  Emacs is simply no fun at all
    if you don't do this!  Ubuntu and Mac OS X make this relatively
    easy.  There is also a registry hack you can Google to do this on
    Windows.

2.  Pick something else to replace Ctrl-X.  The `j` key is a pretty
    good because you can stay on home row and the default key-binding
    for `C-j` isn't like super-important or anything.

        ;; put this in your custom.el if you use QWERTY
        (global-set-key (kbd "C-j") ctl-x-map)

    This little shortcut lets you type `C-j` instead of `C-x`
    automatically without having to rewrite every single key-binding.

    If you use a Dvorak keyboard, the `u` key is an excellent choice!

        ;; put this in "custom.el" this if you use Dvorak
        (global-set-key (kbd "C-u") ctl-x-map)

    Also note that `custom.el` is in the `.gitignore` file so changes
    you make there won't be forced upon anyone else who might be
    cloning your repository.

3.  Consider using `C-h` instead of backspace.  This isn't super
    important, but just help eliminate a little unnecessary reaching.

        (global-set-key (kbd "C-h") 'delete-backward-char)
        (global-set-key (kbd "M-h") 'backward-kill-word)
        ;; GNU readline/Mac OS X have this by default:
        (global-set-key (kbd "C-M-h") 'backward-kill-word)

    If you're using emacs in a terminal, certain ones might put up a
    little bit of a fight.  on mac os there is actually an option to
    make `C-h` function as backspace.

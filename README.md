Spacemacs Config
----------------

To use my .spacemacs, just symlink it into your home directory. The other
folders are layers that can be used by symlinking them into .emacs.d/private and
adding them to dotspacemacs-configuration-layers in your .spacemacs file.

The rtags layer contains all my C++ configs. It's meant to be used as a
standalone C++ layer, so you shouldn't enable the c-c++ layer that comes with
spacemacs.

Note that the mikezackles layer includes support for highlighting generate ninja
files.

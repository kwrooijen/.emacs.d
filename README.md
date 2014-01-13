My dotemacs
===========

This is my emacs config. To see what it is like you can view the latest episodes of my haskell screencasts [here](http://www.youtube.com/lazycasts).

Install
=======

```
git clone git@github.com:AtticHacker/dotemacs
# NOTE: THIS WILL REMOVE YOUR .emacs.d DIRECTORY! YOU SHOULD MOVE YOURS IF YOU ALREADY HAVE ONE
mv dotemacs ~/.emacs.d
```

Once placed you can start emacs and run: M-x -> my-install-packages

This will install all the necessary packages from MELPA.

Once it's done you can restart emacs and it should work as intended, However you might need to install some OS components depending on the packages installed.

Notes
=====

By default the my-extras.el is disabled. This file requires a few packages such as emms (music player), garak (IM client) and w3m (Web browser).
To enable these uncomment (require 'my-extras) in config/my-requires.el. Afterwards you need to restart emacs and run M-x -> my-install-extras-packages.



If you want the same colors as in my screencasts, this is my rxvt-unicode setup:

```
urxvt*font: xft:Monaco:bold:pixelsize=15:antialias=true
urxvt*scrollBar  : false
urxvt*foreground : #f2f2f2
urxvt*background : #000000
urxvt*color0     : #101010
urxvt*color1     : #f13a21
urxvt*color2     : #93f91d
urxvt*color3     : #ffd00a
urxvt*color4     : #004f9e
urxvt*color5     : #ec0048
urxvt*color6     : #2aa7e7
urxvt*color7     : #f2f2f2
urxvt*color8     : #1d202f
urxvt*color9     : #ff361e
urxvt*color10    : #ffc005
urxvt*color11    : #93ff00
urxvt*color12    : #0071ff
urxvt*color13    : #ef0051
urxvt*color14    : #4bb8fd
urxvt*color15    : #a020f0

Xft.dpi       : 96
Xft.antialias : true
Xft.rgba      : rgb
Xft.hinting   : true
Xft.hintstyle : hintslight
```
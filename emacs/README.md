To install (assuming the dotfiles repo is at `~/git/dotfiles`):

```
mkdir ~/.emacs.d
cd ~/.emacs.d
ln -s ~/git/dotfiles/emacs/init.el
ln -s ~/git/dotfiles/emacs/cheat-sheet.txt
```

After installing, `M-x customize`, search for `inhibit-startup-echo-area-message` and customize to
your user name.

I have the following ELPA packages installed:
* `browse-kill-ring`
* `paredit`
* `sublime-themes`
* `cider`
* `cider-browse-ns`

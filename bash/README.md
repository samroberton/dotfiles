Add the following to the bottom of `~/.bashrc` (assuming the `dotfiles` repository is at `~/git/dotfiles`):

```
. ~/git/dotfiles/bash/bashrc
```

(By putting it in `.bashrc`, we get it for both login and non-login shells, assuming that `.profile`  and `.bash_profile` do the standard thing of sourcing `.bashrc` if it exists.)

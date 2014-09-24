To install (assuming the dotfiles repo is at `~/git/dotfiles`), after a `lein` self-install:

```
ln -s ~/git/dotfiles/clojure/lein/profiles.clj ~/.lein/profiles.clj
```

Leiningen:
```
mkdir ~/bin
cd ~/bin
curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod ug+x lein
./lein
```

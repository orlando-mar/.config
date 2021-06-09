# Screenshots

![term](screenshots/terminal.png)

Terminal emulation with `multi-term`

![magit](screenshots/magit.png)

Robust git UI with `magit`

![projectile](screenshots/projectile.png)

Project-based file discovery with `projectile`

![flycheck](screenshots/flycheck.png)

Error checking with `flycheck`

# Setup

This repo contains my emacs and zsh configuration files.

1. `git clone https://github.com/orlando-mar/.orlando-mar-config.git` into your `$HOME` dir
1. create symlinks for the `.zshenv` and `init.el` in your `$HOME` dir
      - `ln -s ~/.orlando-mar-config/emacs/init.el ~/.emacs.d/init.el`
      - `ln -s ~/.orlando-mar-config/zsh/.zshenv .zshenv`

The `zshrc` included will `source` all files in `~/.config/zsh/` for you to add custom configs


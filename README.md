# setup

This repo contains my various configuration files, including my emacs `init.el` and my zsh configs.
You can use these configurations as follows:

1. `git checkout` this repository into your `$HOME` dir
1. create symlinks for the `.zshenv` and `emacs` dirs in your `$HOME` dir. If you are using an XDG-like base dir specification, you may be able to get away with not symlinking the emacs directory, assuming you've properly set `XDG_CONFIG_HOME` to `$HOME/.config` (this is the usual default for OS that use XDG specs). However, the `zshenv` dotfile *almost always* needs to be in your `$HOME` dir
      - `ln -s ~/.config/emacs .emacs.d`
      - `ln -s ~/.config/zsh/.zshenv .zsrhenv`

# main highlights

## emacs

- counsel-mode
- markdown-mode (assumes you're using `multimarkdown` installed through homebrew - you can correct this yourself if that's not true)
- multi-term
- neotree
- nord-theme


# .doom.d config

1.  Install emacs

    brew cask install emacs

2.  Clone this configuration to `~/.doom.d`

    git clone <https://github.com/raydel95/.doom.d.git> ~/.doom.d

3.  Clone doom emacs distribution in `.emacs.d`

    git clone --depth 1 <https://github.com/hlissner/doom-emacs> ~/.emacs.d

    > Check [chemacs](https://github.com/plexus/chemacs) if you want to install it alongside another emacs distro.

4.  Add `$DOOM-PATH/.emacs.d/bin` to your path

    `export PATH="$HOME/.emacs.d/bin:$PATH"`

5.  Run `doom sync`

6.  Start `emacs`

Add this to your zsh config if vterm is used (for more details check [emacs-libvterm](https://github.com/akermu/emacs-libvterm)):

```
# Doom vterm
function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
```

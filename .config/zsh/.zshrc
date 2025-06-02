# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Installed via apt :D
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# Plugin manager
source /usr/share/zplug/init.zsh

# Other plugins
zplug romkatv/powerlevel10k, as:theme, depth:1
zplug zsh-users/zsh-completions
zplug load

# Load completions
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zmodload zsh/complist
compinit
_comp_option+=(globdots)

# Emacs (best) mode
bindkey -e

# history
HISTSIZE=5000
HISTFILE="$XDG_STATE_HOME"/zsh/history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Completion files: Use XDG dirs
[ -d "$XDG_CACHE_HOME"/zsh ] || mkdir -p "$XDG_CACHE_HOME"/zsh
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME"/zsh/zcompcache
compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-$ZSH_VERSION

# Alias
alias remacs="systemctl --user restart emacs"
alias imv="imv-wayland"
alias mpv="swayhide mpv"
alias zathura="swayhide zathura"
alias ls="exa"
alias l="exa --icons -lah"
alias cam="swayhide mpv /dev/video0 --profile=low-latency --untimed"
alias em="emacsclient -t"

#PATH
export PATH="/home/damian/.local/bin:$PATH"
export PATH=$PATH:"/home/damian/.config/composer/vendor/bin/"
source $CARGO_HOME/env
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$XDG_DATA_HOME/go/bin
[ -s "$BUN_INSTALL 
/_bun" ] && source "$BUN_INSTALL/_bun"

# Zoxide
eval "$(zoxide init zsh --cmd cd)"


# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

# bun completions
[ -s "/home/damian/.local/share/bun/_bun" ] && source "/home/damian/.local/share/bun/_bun"

export ZDOTDIR="$HOME/.config/zsh"

# XDG Compliance :P
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# COMPLY WITH THE SYSTEM >:(
export PYTHON_HISTORY="$XDG_STATE_HOME/python/history"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME"/go
export GOMODCACHE="$XDG_CACHE_HOME"/go/mod
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export WORKON_HOME="$XDG_DATA_HOME/virtualenvs"
export BUN_INSTALL="$XDG_DATA_HOME"/bun
export ZPLUG_HOME="$XDG_DATA_HOME"/zplug
export ROSWELL_HOME="$XDG_DATA_HOME"/roswell
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export TEXMFHOME=$XDG_DATA_HOME/texmf
export TEXMFVAR=$XDG_CACHE_HOME/texlive/texmf-var
export TEXMFCONFIG=$XDG_CONFIG_HOME/texlive/texmf-config
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export WORKON_HOME="$XDG_DATA_HOME/virtualenvs"

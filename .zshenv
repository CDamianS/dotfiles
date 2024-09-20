export ZDOTDIR="$HOME/.config/zsh"

# XDG Compliance :P
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# COMPLY WITH THE SYSTEM >:(
export PYTHON_HISTORY="$XDG_STATE_HOME/python/history"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
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

#FIREFOX
export MOZ_ENABLE_WAYLAND=1

# Bemenu config is dumb
export BEMENU_OPTS='--fb "#1e1e2e"\
                    --ff "#cdd6f4"\
                    --nb "#1e1e2e"\
                    --nf "#cdd6f4"\
                    --tb "#1e1e2e"\
                    --hb "#1e1e2e"\
                    --tf "#89b4fa"\
                    --hf "#f9e2af"\
                    --af "#cdd6f4"\
                    --ab "#1e1e2e"\
		    --fn "Arimo Nerd Font"\
		    -H 24
		    -i'

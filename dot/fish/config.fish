# Anton Rybakov fish config

function remove_from_path \
    -a entry
  while set index (contains -i -- $entry $PATH)
    set -eg PATH[$index]
  end
  set -e index
end

function readd_on_top_of_path \
    -a entry
  remove_from_path $entry
  fish_add_path -P $entry
end

function configure_environment_manager \
    -d "Configure tool (e.g. rbenv, pyenv...) in the unified way " \
    -a env_name
  set -f env_root (string upper $env_name)_ROOT
  set -gx $env_root "$HOME/.local/env/$env_name"
  type -q $env_name
  and mkdir -p $env_root
  if test -d $$env_root
    type -q $env_name
    or readd_on_top_of_path "$$env_root/bin"
    type -q $env_name
    and $env_name init - | source
  else
    for var in (set -n)
      string match -q (string upper $env_name)'_*' -- $var
      and set -eg $var
    end
  end
end

# TODO: this is recreation of the pyenv process for cleaning
#       already set pathes by the parent shell processes
#       create review for the rbenv with fix for this
remove_from_path "/home/$USER/.rbenv/shims"
configure_environment_manager rbenv

configure_environment_manager pyenv

set -gx GOENV_GOPATH_PREFIX "$HOME/.local/go"
set -gx GOENV_PATH_ORDER "front"
configure_environment_manager goenv

readd_on_top_of_path "$HOME/.cargo/bin"
readd_on_top_of_path "$HOME/.bin"
readd_on_top_of_path "$HOME/.local/bin"

set -gx EDITOR vim

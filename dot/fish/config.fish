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

function init_env -a env_name
  set -f env_root (string upper $env_name)_ROOT
  set -gx $env_root "$HOME/.local/env/$env_name"
  if test -d $$env_root
    readd_on_top_of_path "$$env_root/bin"
    if type -q $env_name
      $env_name init - | source
    end
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
init_env rbenv

init_env pyenv

set -gx GOENV_GOPATH_PREFIX "$HOME/.local/go"
set -gx GOENV_PATH_ORDER "front"
init_env goenv

readd_on_top_of_path "$HOME/.cargo/bin"
readd_on_top_of_path "$HOME/.bin"
readd_on_top_of_path "$HOME/.local/bin"

set -gx EDITOR vim

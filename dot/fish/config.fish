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

# rbenv
# TODO: this is recreation of the pyenv process for cleaning
#       already set pathes by the parent shell processes
#       create review for the rbenv with fix for this
if type -q rbenv
  remove_from_path "/home/$USER/.rbenv/shims"
  rbenv init - | source
end

# pyenv
if type -q pyenv
  pyenv init - | source
end

# homebrew .bin to PATH
readd_on_top_of_path "$HOME/.cargo/bin"
readd_on_top_of_path "$HOME/.bin"
readd_on_top_of_path "$HOME/.local/bin"

set -gx EDITOR vim

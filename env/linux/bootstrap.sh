#!/bin/sh

create_dir () {
  test "$#" -ne 1 && { printf "Invalid amount of arguments\n"; return 1; }
  directory="$1"
  if ! test -d "$directory"
  then
    printf 'Creating: %s\n' "$directory"
    mkdir -p "$directory"
  else
    printf 'Already exists: %s\n' "$directory"
  fi
}

clone_repo () {
  test "$#" -ne 2 && { printf "Invalid amount of arguments\n"; return 1; }
  repo="$1"
  root="$2"
  if ! test -d "$root"
  then
    printf 'Cloning: %s -> %s\n' "$repo" "$root"
    git clone "$repo" "$root"
  else
    printf 'Already exists: %s\n' "$root"
  fi
}

printf 'User: %s\n' "$USER"
printf 'Home: %s\n' "$HOME"

env_folder="$HOME/.local/env"
create_dir "$env_folder"

rbenv_repo="https://github.com/rbenv/rbenv"
rbenv_root="$env_folder/rbenv"
clone_repo "$rbenv_repo" "$rbenv_root"

ruby_build_repo="https://github.com/rbenv/ruby-build"
ruby_build_root="$rbenv_root/plugins/ruby-build"
clone_repo "$ruby_build_repo" "$ruby_build_root"

pyenv_repo="https://github.com/pyenv/pyenv"
pyenv_root="$env_folder/pyenv"
clone_repo "$pyenv_repo" "$pyenv_root"

goenv_repo="https://github.com/go-nv/goenv"
goenv_root="$env_folder/goenv"
clone_repo "$goenv_repo" "$goenv_root"

function vims \
    --description "Vim sessions are closer then you think!" \
    --argument session

  if test -z $session
    ls $HOME/.vim_sessions | sed 's/.vim//g'
    return 0
  end

  set session_file "$HOME/.vim_sessions/$session.vim"
  if not test -f $session_file
    printf "session file ($session_file) does not exist\n" >&2
    return 1
  end

  vim -S $session_file

end

function vims \
    --description "Vim sessions are closer then you think!" \
    --argument session

  if test -z $session
    ls ~/.vim_sessions | sed 's/.vim//g'
    return 0
  end

  set session_file "~/.vim_sessions/$session"
  if not test -f $session_file
    printf "session file does not exist\n" >&2
    return 1
  end

  vim -S ~/.vim_sessions/$session.vim

end

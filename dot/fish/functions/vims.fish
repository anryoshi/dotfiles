function vims \
  --description "Vim Superiority!"

  # This is a vim wrapper script for convenient handling
  # servers and sessions
  # 
  # In case of absence explicit servername and sessionname
  # parameter following behavior is observed.
  # 
  # If there isn't any running server - create a new one with default name.
  # If there is only one running server - use it.
  # If there are multiple running servers - ask which one to use.
  # 
  # If creating a new server and session is not provided use 'main'.
  # If sending to existing server - ignore session parameter completely.

  function __cleanup --on-event fish_postexec
    functions -e __cleanup
    functions -e __print_help
    functions -e __verbose
    set -e __verbose_enabled
  end

  function __print_help
    begin
      printf "Usage:\n"
      printf "  vims [OPTIONS]\n\n"
      printf "Options:\n"
      printf "  -h, --help           Show this help message and exit\n"
      printf "  -v                   Verbose output\n"
      printf "  -l                   Print list of available sessions\n"
      printf "      --nogui          Run without GUI\n"
      printf "      --servername VAL Set the server name\n"
      printf "      --session VAL    Set the session name\n"
    end >&2
  end

  set -g __verbose_enabled false
  function __verbose
    $__verbose_enabled 
    and printf $argv >&2
  end

  argparse 'h/help' 'v' 'l' 'nogui' 'servername=' 'session=' -- $argv
  or return

  if set -ql _flag_h
    __print_help
    return 1
  end

  set -ql _flag_v
  and set -g __verbose_enabled true

  set -l sessions_dir "$HOME/.local/state/vim/sessions"
  if set -ql _flag_l
    ls $sessions_dir | sed 's/.vim//g'
    return 0
  end

  set -f vim_client gvim
  set -ql _flag_nogui
  and set -f vim_client vim

  set -f serverlist (vim --serverlist)

  if set -ql _flag_servername[1]
    set -f servername $_flag_servername[1]
  else if test (count $serverlist) -eq 1
    set -f servername $serverlist[1]
  else if test (count $serverlist) -eq 0
    set -f servername MAIN
  else
    __verbose "Failed to deduce server name - multiple servers are running\n"
    return 1
  end

  if contains $servername $serverlist
    if test (count $argv) -gt 0
      __verbose "Sending %s to server %s\n" $argv $servername
      $vim_client --servername $servername --remote $argv
    else
      __verbose "Nothing to do\n"
    end
    return 0
  end

  set -f session main
  set -ql _flag_session[1]
  and set -f session $_flag_session[1]

  set -l session_file "$sessions_dir/$session.vim"

  set -f session_arg "+silent Obsession $session_file"
  test -f $session_file
  and set -f session_arg -S $session_file

  __verbose "Starting server %s with session %s\n" $servername $session
  $vim_client --servername $servername $session_arg
  if test (count $argv) -gt 0
    __verbose "Sending %s to server %s\n" $argv $servername
    $vim_client --servername $servername --remote $argv
  end
end

function l \
    --description 'Meaningful ls defaults'
  set -f gnu_ls_options -AFGhlv --time-style=long-iso --group-directories-first
  switch (uname)
    case "Linux"
      ls $gnu_ls_options $argv
    case "Darwin"
      if command -sq "gls"
        # GNU ls version is available
        gls $gnu_ls_options $argv
      else
        # Only BSD ls version is available
        ls -a -F $argv
      end
  end
end

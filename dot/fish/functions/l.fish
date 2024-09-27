function l \
    --description "Meaningful ls defaults"
  switch (uname)
    case "Linux"
      ls -a -F -v --group-directories-first $argv
    case "Darwin"
      if command -sq "gls"
        # GNU ls version is available 
        gls -a -F -v --group-directories-first $argv
      else
        # Only BSD ls version is available
        ls -a -F $argv
      end
  end
end

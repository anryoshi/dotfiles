function fup \
    -a file \
    -d "Search for file recursevly to root from cwd, and cd to this location"
  if test "$file" = ""
    printf "Provide file to search\n" >&2
    return
  end
  set -l cwd (pwd)
  set -l dir (pwd)

  while not test "$dir" = '/'
    set -l file_to_find "$dir/$file"

    if test -f "$file_to_find"
      echo "$dir/$file"
      break
    end

    cd $dir/..
    set dir (pwd)
  end

  cd $cwd
end

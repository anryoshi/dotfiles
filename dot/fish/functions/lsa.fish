function lsa --description "Show all files in C locale"
    set -l ls_program "ls"
    set -l gnu_additional_options "--color=always" "--group-directories-first"
    if test (uname) = "Darwin" 
        if command -sq "gls"
            set ls_program "gls"
        else
            set gnu_additional_options
        end
    end
    env LC_ALL=C $ls_program -a -1 $gnu_additional_options $argv
end

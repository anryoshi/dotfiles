function fish_prompt
    set -f previous_status $status

    set -f login_str ""
    if set -q SSH_CONNECTION
        set -g fish_color_user yellow
        set -g fish_color_host red
        set -g fish_color_host_remote red

        set -f login_str (prompt_login)" "
    end

    set -f cwd_str (set_color cyan)(prompt_pwd -d 0 -D 99)(set_color normal)

    set -f separator_color green
    test $previous_status -ne 0 
    and set -f separator_color red
    set -f separator_str (set_color $separator_color)"❯ "(set_color normal)

    set -g __fish_git_prompt_show_informative_status 1
    set -g __fish_git_prompt_describe_style branch
    set -g __fish_git_prompt_showupstream auto
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showcolorhints 1
    set -f repo_str (fish_git_prompt)
    or set -f repo_str ""

    printf "%s%s%s\n%s" $login_str $cwd_str $repo_str $separator_str
end

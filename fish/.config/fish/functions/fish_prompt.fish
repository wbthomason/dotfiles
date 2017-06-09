function fish_prompt --description 'Write out the prompt'
    # User
    set_color $fish_color_user
    echo -n (whoami)
    set_color normal

    echo -n '@'

    # Host
    set_color $fish_color_host
    echo -n (prompt_hostname)
    set_color normal

    echo -n ':'

    # PWD
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color normal
    
    #echo -n ' '
    # Do nothing if not in vi mode
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
        or test "$fish_key_bindings" = "fish_hybrid_key_bindings"
        switch $fish_bind_mode
            case default
                set_color --bold red 
            case insert
                set_color --bold green 
            case replace-one
                set_color --bold  blue 
            case visual
                set_color --bold  yellow 
        end
    end

    echo -n ' '
    set_color normal
    echo -n ' '
end

function fish_right_prompt --description 'Write out the right prompt'
	set -l last_status $status
    git_prompt
    __fish_hg_prompt
    echo

    if not test $last_status -eq 0
        set_color $fish_color_error
    end
end

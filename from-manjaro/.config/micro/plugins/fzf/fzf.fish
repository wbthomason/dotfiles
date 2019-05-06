function fzf
    set -l epoch (date "+%s")
    set -l file_path $TMPDIR/fzf-$epoch.result
    command fzf $argv >$file_path
    if test $status -eq 0 -a -s $file_path
        cat $file_path
    end
    if test -e $file_path
        rm $file_path
    end
end

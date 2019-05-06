# About the fzf plugin

This plugin provides support for opening files via [fzf](https://github.com/junegunn/fzf).
# Commands

The plugin providies the following commands:

* `fzf`: launch `fzf` to find a file to open.


# Troubleshooting

There is a [known issue](https://github.com/fish-shell/fish-shell/issues/1362) when using fzf with fish shell. To work around this, you should create a new fish shell function called `fzf` to be trigged instead of the `fzf` command directly. You can copy and past the following snippet into the file:

$FISH_CONFIG_PATH/functions/fzf.fish

```
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
```



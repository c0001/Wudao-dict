#!/bin/bash

[[ ! `ps -ef | grep "python3 WudaoServer.py" | grep -v "grep" | wc -l` -eq 0 ]]\
    && wd -k

local_bashcompletion_dir=${BASH_COMPLETION_USER_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion}/completions

[[ -f ~/.local/bin/wd ]] && rm -f ~/.local/bin/wd
[[ -f $local_bashcompletion_dir/wd ]] && rm -f $local_bashcompletion_dir/wd

echo 'Uninstall Finished! '


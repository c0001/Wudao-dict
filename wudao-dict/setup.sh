#!/bin/bash
wd_sourcedir="${BASH_SOURCE[0]}"
while [ -h "$wd_sourcedir" ]; do # resolve $wd_sourcedir until the file is no longer a symlink
    wd_dir="$( cd -P "$( dirname "$wd_sourcedir" )" >/dev/null && pwd )"
    wd_sourcedir="$(readlink "$wd_sourcedir")"

    # if $wd_sourcedir was a relative symlink, we need to resolve it relative
    # to the path where the symlink file was located
    [[ $wd_sourcedir != /* ]] && wd_sourcedir="$wd_dir/$wd_sourcedir" 
done
wd_dir="$( cd -P "$( dirname "$wd_sourcedir" )" >/dev/null && pwd )"

# 用户词
if [ ! -d usr ]
then
    mkdir usr
fi

chmod -R 777 usr

sysOS=`uname -s`
local_bashcompletion_dir=${BASH_COMPLETION_USER_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion}/completions

function JudgeDo ()
{
    local prompt="$1"
    local choice
    read -p "$prompt" choice;
    if [[ $choice == 'y' ]];then
        echo t
    else
        echo nil
    fi
}

function RegisteWD ()
{
    mkdir -p ~/.local/bin
    rm -f ~/.local/bin/wd
    cd ~/.local/bin && ln -s $wd_dir/wd ./wd && cd $wd_dir
    chmod +x ~/.local/bin/wd
}

RegisteWD

if [[ $(JudgeDo "Add bash auto completion for 'wd'? ") == 't' ]]
then
    mkdir -p ${local_bashcompletion_dir}
    rm -f ${local_bashcompletion_dir}/wd
    cd ${local_bashcompletion_dir} && ln -s $wd_dir/wd_com ./wd && cd $wd_dir
    . ${local_bashcompletion_dir}/wd
fi

echo 'Setup Finished! '
echo 'use wd [OPTION]... [WORD] to query the word.'
echo '自动补全(仅支持bash)会在下次打开命令行时启用'
echo '或者手动运行 source ~/.bashrc'

#!/bin/bash

# 用户词
if [ ! -d usr ]
then
    mkdir usr
fi

chmod -R 777 usr

# 添加系统命令wd
echo '#!/bin/bash'>./wd
echo 'save_path=$PWD'>>./wd
echo 'cd '$PWD >>./wd
echo './wdd $*'>>./wd
echo 'cd $save_path'>>./wd

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
    cp ./wd ~/.local/bin/wd
    chmod +x ~/.local/bin/wd
}

RegisteWD

if [[ $(JudgeDo "Add bash auto completion for 'wd'? ") == 't' ]]
then
    mkdir -p ${local_bashcompletion_dir}
    rm -f ${local_bashcompletion_dir}/wd
    cp wd_com ${local_bashcompletion_dir}/wd
    . ${local_bashcompletion_dir}/wd
fi

echo 'Setup Finished! '
echo 'use wd [OPTION]... [WORD] to query the word.'
echo '自动补全(仅支持bash)会在下次打开命令行时启用'
echo '或者手动运行 source ~/.bashrc'

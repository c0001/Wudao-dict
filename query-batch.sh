#!/bin/bash

# See https://stackoverflow.com/a/246128/3561275
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"


CALL_PWD=$(pwd)

dict_f=$DIR/query.dict
desc_f=$DIR/query.cache_desc
full_f=$DIR/query.cache

function FAuto_delete ()
{
    if [[ -f $1 ]]
    then
        echo -e "\e[33mFile '$1' exists, deleting ...\e[0m"
        rm $1
        [[ ! $? -eq 0 ]] && echo -e "\e[31mFatal when rm '$1', abort.\e[0m" && exit
    fi
}

function Fexists_error ()
{
    if [[ ! -f $1 ]]
    then
        echo -e "\e[31mError: can not find file '$1' .\e[0m"
        exit
    fi
}

function Fcommand_error ()
{
    local type=$(command -v $1)
    if [[ ! $? -eq 0 ]]
    then
        echo -e "\e[31mCommand not found '$1'\e[0m"
        exit
    fi
}

Fcommand_error wd
FAuto_delete $desc_f
FAuto_delete $full_f
Fexists_error $dict_f

function FQuery_response ()
{
    touch $desc_f $full_f
    local IFS=$'\n'
    local words=$(cat $dict_f)
    local item
    for item in $words
    do
        if [[ ! -z $item ]]
        then
            echo -e "\e[32mQuery words:\e[0m \e[33m<$item>\e[0m"
            
            # query short translation
            echo "**********<<$item>>**********" >> $desc_f
            echo -e "$(wd -s $item)" >> $desc_f
            echo "<<********************>>" >> $desc_f
            echo -e "\n" >> $desc_f

            # query full translation
            echo "**********<<$item>>**********" >> $full_f
            echo -e "$(wd $item)" >> $full_f
            echo "<<********************>>" >> $full_f
            echo -e "\n" >> $full_f
        fi
    done
}

FQuery_response

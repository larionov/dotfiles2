#!/bin/bash
shopt -s dotglob

for i in *
do
    if [ ".git" == $i ]; then continue; fi
    if [ ".config" == $i ]; then continue; fi
    if [ -d ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -r ~/$i
    fi
    echo "linking..."  $(realpath ./$i) $(realpath ~/$i)
    ln -s $(realpath ./$i) $(realpath ~/$i)
    
    if [ -f ~/$i ]
    then
        echo "deleting file ..." $(realpath ~/$i)
        rm -r ~/$i
    fi
    echo "linking..." $(realpath ./$i)
    ln -s $(realpath ./$i) $(realpath ~/$i)
done

for i in .config/*
do
    if [ -d ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
    fi
    echo "linking..."  $(realpath ./$i) $(realpath ~/$i)
    ln -s $(realpath ./$i) $(realpath ~/$i)

    if [ -f ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
    fi
    echo "linking..." $(realpath ./$i)
    ln -s $(realpath ./$i) $(realpath ~/$i)
done

#!/bin/bash
shopt -s dotglob

for i in *
do
    if [ ".git" == $i ]; then continue; fi
    if [ ".config" == $i ]; then continue; fi
    if [ -d ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
        echo "linking..."  $(realpath ./$i) $(realpath ~/$i)
        ln -s $(realpath ./$i) $(realpath ~/$i)
    fi
    if [ -f ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
        echo "linking..." $(realpath ./$i)
        ln -s $(realpath ./$i) $(realpath ~/$i)
    fi
done

for i in .config/*
do
    if [ -d ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
        echo "linking..."  $(realpath ./$i) $(realpath ~/$i)
        ln -s $(realpath ./$i) $(realpath ~/$i)
    fi
    if [ -f ~/$i ]
    then
        echo "deleting..." $(realpath ~/$i)
        rm -rI ~/$i
        echo "linking..." $(realpath ./$i)
        ln -s $(realpath ./$i) $(realpath ~/$i)
    fi
done

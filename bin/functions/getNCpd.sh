#!/bin/bash

BASE_DIR=$1

SIMULTANEOUS_PROCESSES=$2 #1

# work directory
WORKDIR=${BASE_DIR}"data"

#set separator character
IFSAnt=$IFS
IFS=$'\n'

file="/home/clovis/Dropbox/projetos/Igor/gitAPs/data/database/cpdexcluded.txt"
dest="/home/clovis/Dropbox/projetos/Igor/gitAPs/data/database/cpdexcluded2.txt"

for i in $(cat $file); do
    curl -s "https://www.genome.jp/dbget-bin/www_bfind_sub?mode=bfind&max_hit=1000&locale=en&serv=kegg&dbkey=compound&keywords=$i&page=1" -o ./"tmp"
    name=$(grep '<a href="/dbget-bin/www_bget?cpd:' tmp|sed -r 's/.+left:2em">(.+)<\/div><\/div><\/form><hr>/\1/')
    echo $i':|:'$name >> $dest
done
    

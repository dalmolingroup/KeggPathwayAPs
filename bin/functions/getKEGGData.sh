#!/bin/bash

#==============================================================
#**********
#**********  IGOR BRANDAO / Clovis Reis 2020
#**********
#**********  PGM: SCRIPT TO DOWNLOAD ALL KEGG PATHWAYS
#**********
#**********  VERSION:	2.0
#**********
#**********  NOV/2019 - Creation
#**********
#==============================================================

#==============================================================
# GLOBAL VARIABLES
#==============================================================

BASE_DIR=$1

SIMULTANEOUS_PROCESSES=$2 #1

# work directory
WORKDIR=${BASE_DIR}"data/kgml/"

# Data type
DATA_TYPE=$3 # : ec for ec and ko for ko

# list of organims
# edit lstSomeOrgs for personalized organims
ORGANISMS=$4 #"s" # s for some or a for all

#set separator character
IFSAnt=$IFS
IFS=$'\n'

function download {
#==============================================================
# DOWNLOAD KEGG FILES
#==============================================================

# go to work dir
cd ${WORKDIR}

echo "<<< INITIALIZING KEGG KGML FILES DOWNLOAD..."

case ${DATA_TYPE} in
    "ec")
        # Generate current organism folder name
        org_folder="${DATA_TYPE}"

        # Check if the browser folder exists
        if [ ! -d $org_folder ]; then
            mkdir -p $org_folder;
        fi

        # Enter into the org folder data
        cd $org_folder

        # Perform the KGML file download
        for lines in $(curl -s "http://rest.kegg.jp/list/pathway/ec"); do
            echo "Downloading $lines"
            file=$(echo $lines | awk '{split($1,a,":"); print a[2]}')
            curl -s "http://rest.kegg.jp/get/$file/kgml" -o ./"$file.xml"
        done
        ;;
    "ko")
        # Iterate over the organism list
        for i in "${!ORG_LIST[@]}"; do
            export ${ORG_LIST[$i]}
            echo "***************"
            echo ${ORG_LIST[$i]}
            echo "***************"

            # Generate current organism folder name
            org_folder="${ORG_LIST[$i]}"

            # Check if the browser folder exists
            if [ ! -d $org_folder ]; then
                mkdir -p $org_folder;
            fi

            # Enter into the org folder data
            cd $org_folder

            # Perform the KGML file download
            for lines in $(curl -s "http://rest.kegg.jp/list/pathway/${ORG_LIST[$i]}"); do
                echo "Downloading $lines"
                file=$(echo $lines | awk '{split($1,a,":"); print a[2]}')
                #echo $file 
                curl -s "http://rest.kegg.jp/get/$file/kgml" -o ./"$file.xml"
            done

            # return to work dir
            cd ${WORKDIR}
        done
        ;;
    *) echo "Invalid option for data type list! Use <ec> for ec or <ko> for ko"
        exit
        ;;
esac

echo "<<< PROCESS FINISHED SUCCESSFULLY!"
}
#==============================================================



#==============================================================
# MAIN
#==============================================================

case $ORGANISMS in
    "some") source ${BASE_DIR}"bin/functions/lstSomeOrgs"
        ;;
    "all") source ${BASE_DIR}"bin/functions/lstAllOrgs"
        ;;
    *) echo "Invalid option for organims list! Use <some> for the organims inside lstSomeOrgs file or <all> for all organims in KEGG"
        exit
        ;;
esac 


# Iterate over the organism list
for((i=1; i <= "${SIMULTANEOUS_PROCESSES}"; i++)); do 
    download & 
done
wait

IFS=$IFSAnt
#==============================================================

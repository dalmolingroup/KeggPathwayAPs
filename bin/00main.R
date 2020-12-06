#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#*************************************************
# Updated in 19/11/2020
#*************************************************

#main ----


# ver rota 00061

#Clean all variables ----
rm(list=ls(all=TRUE))

#Base location ----
#Did you change it to your base location?
dirBase<-"Place here the correct name of your work folder"

dirBase<<-"/home/clovis/Dropbox/projetos/Igor/gitAPs"

#figures
dirFig<<-file.path(dirBase,"figures")
#bin dir
binDir<<-file.path(dirBase,"bin")
#function dir
funcDir<<-file.path(binDir,"functions")
#database folder and file
dbDir<<-file.path(dirBase,"data","database")
dbFile<<-file.path(dbDir,"dictionary.db")


#Organisms to process
#Put here your list of organisms inside the '...' separated by space and "..." as delimiter
orgList<-'"hsa" "mmu"'

# Import dependencies file to load dependencies and functions
source(file.path(funcDir,"dependencies.R"))
loadDependencies()

# Create the organisms list to be processed
createOrgList(funcDir = funcDir, orgList = orgList)

# Download the XML files from KEGG
# Parameters:
#   dirBase - name of your work folder
#   simProcesses - number of simultaneous download processes
#   dataType - KEGG database to download. Use "ec" for ec and "ko" for ko
#   organisms - list of organisms to download. Use <some> for the organisms 
#     inside your orgList or <all> for all organisms in KEGG.
#     Edit the orgList above for personalized organisms
#   skip - don't execute this phase and a downloaded data
#     to execute your own download change skip to F


# The ec XML files  are necessary to generate the dictionary of nodes.
# Skip if using our pre downloaded ec XML files available on github
downloadKGML(dirBase = dirBase, 
             simProcesses = 1, 
             dataType = "ec", 
             skip = T) 


# Download the organims XML files
# Skip if using our hsa and mmu pre downloaded XML files available on github
downloadKGML(dirBase = dirBase, 
             simProcesses = 1, 
             dataType = "ko",
             organisms = "some", 
             skip = T) 

#create a new empty database
# WARNING: all data will be lost
createDB(skip = T)

# read xml files and load data to database
generateDataFromKGML(dirBase = dirBase,
                     dataType = 'ec',
                     skip = T)

createNodesFromEC(dirBase = dirBase, skip = T)

showGraph(pathway = "ec00300", plot = T)

ecs<-c('ec:5.1.3.3','ec:2.7.1.147',
              'ec:5.1.3.15','ec:5.3.1.9','ec:2.7.1.199',
              'ec::2.7.1.1','ec:3.1.3.10',
              'ec:3.1.3.9','ec:5.4.2.2')
showGraph(pathway = "ec00010", ecs = ecs, plot = T)

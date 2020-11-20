#*************************************************
#main ----
#
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


#Clean all variables ----
rm(list=ls(all=TRUE))

#Base location ----
#Did you change it to your base location?
dirBase<-"Place here the correct name of your work folder"

dirBase<-"/home/clovis/Projetos/GrupoDalmolin/Igor/gitAPs/"

#figures
dirFig<-file.path(dirBase,"figures/")
#bin dir
binDir<-file.path(dirBase,"bin/")
#function dir
funcDir<-file.path(binDir,"functions/")

# Import dependencies file to load dependencies and functions
source(file.path(funcDir,"dependencies.R"))
loadDependencies()

#Download the XML files from KEGG
#Parameters:
#   dirBase - name of your work folder
#   simProcesses - number of simultaneous download processes
#   dataType - KEGG database to download. Use "ec" for ec and "ko" for ko
#   organisms - list of organisms to download. Use <some> for the organisms 
#     inside lstSomeOrgs file or <all> for all organisms in KEGG.
#     Edit lstSomeOrgs file for personalized organisms
#   skip - don't execute this phase and use our downloaded data available on github
#     to execute your own download change skip to F
downloadKGML(dirBase = dirBase, 
             simProcesses = 1, 
             dataType = "ko",
             organisms = "some", 
             skip = T) 




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

# Import functions files
source(file.path(funcDir,"graphFunctions.R"))
source(file.path(funcDir,"kgmlFunctions.R"))
source(file.path(funcDir,"helperFunctions.R"))
source(file.path(funcDir,"dependencies.R"))

loadDependencies()




#******************************************#
# Pipeline to generate the static networks #
#******************************************#

# ---- IMPORT SECTION ----

# Clean all variables
rm(list=ls(all=TRUE))

# Import the necessary libraries
library(dplyr)

# Graph handling
library(igraph)
library(visNetwork)

# Graph plot
library(ggraph)

# Graph layouts
library(graphlayouts)
library(oaqc)

# Color pallete and scale
library(RColorBrewer)
library(viridis)
library(scales)

# Image export
library(svglite)

# Base location ----
# Did you change it to your base location?
dirBase<-"Place here the correct name of your work folder"
dirBase<<-"/media/igorabrandao/IGOR BACKUP SERVER/Arquivos Igor/Faculdades/UFRN/4 - Mestrado/Pesquisas/System biology approaches in the investigation of bottlenecks in KEGG pathways/KeggPathwayAPs"
setwd(dirBase)
getwd()

#figures
dirFig<<-file.path(dirBase,"figures")
#bin dir
binDir<<-file.path(dirBase,"bin")
#function dir
funcDir<<-file.path(binDir,"functions")
#database folder and file
dbDir<<-file.path(dirBase,"data","database")
dbTemplate <- file.path(dbDir,"APs.sql")
dbFile<<-file.path(dbDir,"dictionary.db")

# Import dependencies file to load dependencies and functions
source(file.path(funcDir,"dependencies.R"))
loadDependencies()

# Import the graphLoader functions
files.sources = NULL
files.sources[1] = paste0(funcDir, "/", "staticGraph.R")
sapply(files.sources, source)

#*******************************************************************************************#

# ---- SETTINGS SECTION ----

#*************************#
# Pipeline basic settings #
#*************************#

# Load the pathways by organisms data
orgList <- c("ec", "hsa", "mmu", "dme", "sce", "cel")
#orgList <- getOrgCounts()
#orgList <- orgList$org

# Create tge DB connection
createDbConnection()

# Load the paythway list
pathwayList <- getAllPathways()

# Reduce the pathway list for test purpose
pathwayList = pathwayList[1:5]

#*******************************************************************************************#

# ---- PIPELINE SECTION ----

#***************#
# Pipeline flow #
#***************#

# The pipeline itself is not working
# Please go into staticGraph.R and run
# the commands manually

# Loop 01: Run through all available organisms
lapply(orgList, function(org_) {
  pathway_index <- 1
  
  # Loop 02: Run through all available pathways
  lapply(pathwayList, function(pathway_) {
    createDbConnection()
    
    # Adjust the pathway code
    pathway_ = str_replace(pathway_, 'ec', '')
    
    # Status message
    printMessage(paste0("GENERATING ORG ", org_, ", PATHWAY ", pathway_, " INTERATIVE NETWORK [", pathway_index, " OF ", length(pathwayList), "]"))
    
    # Just execute if the network don't exist
    if (!file.exists(file.path(paste0(dirBase, '/output/network/static/', org_, '/', pathway_, '.png')))) {
      # Generate the static network
      generatedNetwork <- showStaticGraph(pathway_ = pathway_, org_ = org_, 
                                           auxInfo_ = T, label_ = "enzyme", 
                                           removeFake_ = T, customLayout_="gem")
      
      if (!is.null(generatedNetwork)) {
        exportNetwork(org_, pathway_)
      } else {
        printMessage(paste0("Organism doesn't have this pathway, skipping it..."))
      }
    } else {
      printMessage(paste0("Network already exists, skipping it..."))
    }
    
    # Increment the index
    pathway_index <<- pathway_index + 1
    
  }) # End of Loop 02
  
}) # End of Loop 01

# End ----
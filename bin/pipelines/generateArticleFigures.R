#*******************************************#
# Pipeline to generate the dynamic networks #
#*******************************************#
 
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
dirBase<<-"/home/clovis/Doutorado/Projetos/Igor/gitAPs"

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
files.sources[1] = paste0(funcDir, "/", "dynamicGraph.R")
sapply(files.sources, source)

#*******************************************************************************************#

# ---- SETTINGS SECTION ----

#*************************#
# Pipeline basic settings #
#*************************#

# Load the pathways by organisms data
orgList <- c("hsa", "mmu", "dme", "sce", "cel","ath")
#orgList <- getOrgCounts()
#orgList <- orgList$org

# Create tge DB connection
createDbConnection()

# Load the paythway list
pathwayList <- getAllPathways()

# Reduce the pathway list for testint purpose
#pathwayList = pathwayList[1:5]

#*******************************************************************************************#

# ---- PIPELINE SECTION ----

#***************#
# Pipeline flow #
#***************#
#org_ <- "ec"#debug
# Loop 01: Run through all available organisms
# lapply(orgList, function(org_) {
#define the lenght of edges. can be 'long' 'medium' 'short' 
edgeLength = 'short' 

pathway_index <- 1

# Just path 00010
pathway_ <- pathwayList[[1]]
createDbConnection()

# Adjust the pathway code
pathway_ = str_replace(pathway_, 'ec', '')

# Status message
# printMessage(paste0("GENERATING ORG ", org_, ", PATHWAY ", pathway_, " INTERATIVE NETWORK [", pathway_index, " OF ", length(pathwayList), "]"))
printMessage(paste0("GENERATING PATHWAY ", pathway_, " INTERATIVE NETWORK [", pathway_index, " OF ", length(pathwayList), "]"))

# Generate the dynamic network

#nodes to delete
delNodes <- c('1.1.1.1+','1.1.5.5','1.2.1.3','1.2.1.5+','1.2.4.1','1.8.1.4','2.3.1.12','2.7.1.1+','2.7.1.1+_1','2.7.1.199','3.1.3.9','3.1.3.10','3.2.1.86','3.2.1.86_1','4.1.1.1','5.1.3.3','5.1.3.15+','5.3.1.9','5.3.1.9_1','5.4.2.2+','6.2.1.1','6.2.1.13')
gEC <- getGraph(pathway_ = pathway_,
                org_ = 'ec')
gBase <- getDynamicGraph(pathway_ = pathway_,
                         org_ = 'ec',
                         gEC,
                         edgeLength = edgeLength)

delNodesId<-gBase$x$nodes$nId[gBase$x$nodes$label %in% delNodes]
gBase$x$nodes<-gBase$x$nodes[!gBase$x$nodes$nId %in% delNodesId,]
gBase$x$edges<-gBase$x$edges[!gBase$x$edges$from %in% delNodesId,]
gBase$x$edges<-gBase$x$edges[!gBase$x$edges$to %in% delNodesId,]

exportNetwork(gBase, pathway_, 'ecShort')

idx = 1
for (idx in 1:length(orgList)) {
  tmp <- getGraph(pathway_ = pathway_,
                  org_ = orgList[idx])
  if (!is.null(tmp)) {
    tmp <- getDynamicGraph(pathway_ = pathway_,
                           org_ = orgList[idx],
                           tmp,
                           edgeLength = edgeLength)
    tmp$x$nodes<-tmp$x$nodes[!tmp$x$nodes$nId %in% delNodesId,]
    tmp$x$edges<-tmp$x$edges[!tmp$x$edges$from %in% delNodesId,]
    tmp$x$edges<-tmp$x$edges[!tmp$x$edges$to %in% delNodesId,]
    tmp <- hideNodes(base = gBase , 
                     graph = tmp,
                     orgList[idx])
    exportNetwork(tmp, pathway_, paste0(orgList[idx],'Short'))
  } else {
    printMessage(paste0("Organism doesn't have this pathway, skipping it..."))
  }
}

# Increment the index
pathway_index <<- pathway_index + 1


# End ----

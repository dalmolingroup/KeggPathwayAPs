#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#******************************************#
# Functions to control the pipeline phases #
#******************************************#

# phaseFunctions.R #

# ---- IMPORT SECTION ----

#' This is the set of functions to handle the KEGG kgml files
#'
#' @author
#' Igor Brandão / Clovis Reis

# Phase 01 ----
# Download XML files from KEGG
downloadKGML <- function(dirBase,
                         simProcesses = 1,
                         dataType = "ec",
                         organisms = "some",
                         skip = T){
  #Parameters:
  #   dirBase - name of your work folder
  #   simProcesses - number of simultaneous processes
  #   dataType - KEGG databese to download. Use ec for ec and ko for ko
  #   organisms - list of organisms to download. Use <some> for the organims 
  #     inside lstSomeOrgs file or <all> for all organims in KEGG
  #   skip - don't execute this phase
  
  if(skip){
    cat("Skiping KEGG download... \n\n")
    return(0)
  }
  
  cat("Starting download process. It will take a while... \n\n")
  
  #function dir
  funcDir<-file.path(dirBase,"/bin/functions/")
  
  exec<-paste0(funcDir,"getKEGGData.sh")
  command<-paste(exec,dirBase, simProcesses, dataType, organisms)
  system(command)
}


generateDataFromKGML <- function(dirBase,
                        dataType = "ec",
                        skip = T) {
  #Parameters:
  #   dirBase - name of your work folder
  #   dataType - Use ec to porcess ec pathways and ko for ko pathways
  #   skip - don't execute this phase
  
  if(skip){
    cat("Skiping", dataType,  "pathways processing... \n\n")
    return(0)
  }
  
  cat("Starting", dataType,  "pathways processing... \n\n")
  
  workDir<-file.path(dirBase,"data","kgml")
  
  if
  # Reference pathway
  reference_pathway <- dataType
  
  # Get the list of files
  folder = file.path("./output/kgml/", reference_pathway, "/")
  #kgml_list <- list.files(path=folder, pattern='*.xml')
  
  #*********************************************************************************#
  # IMPORTANTE: Aqui estou reduzindo a lista das pathways para não ficar tão pesado!
  #*********************************************************************************#
  kgml_list = c('ec00010.xml', 'ec00520.xml')
  kgml_list = c('ec00520.xml')
  
  kgml_index <- 1
  
  # Define the number of available pathways
  available_pathways <- length(kgml_list)
  
  # Check if the folder contains files
  if (is.null(kgml_list) | length(kgml_list) == 0) {
    # Status message
    printMessage("There aren't available pathways...")
    return(FALSE)
  }
  file = kgml_list[1]
  # Loop 01: Run through all available pathways kgml
  lapply(kgml_list, function(file) {
    
    # Load the dataframe
    current_kgml <- KGML2Dataframe(paste0(folder, file))
    save(current_kgml,file="/home/clovis/Projetos/GrupoDalmolin/Igor/essencialidade/bin/calculoAP/kgml.RData")
    
    # Get the pathway code
    pathway_code <- onlyNumber(file)
    
    # Status message
    printMessage(paste0("COUNTING ", pathway_code, " ENZYMES FREQUENCIES [", kgml_index, " OF ", available_pathways, "]"))
    
#    tryCatch({
      # Convert the pathway data into a graph
      pathwayGraphOrig <- KGML2Graph(paste0(folder, file), replaceOrg=TRUE, orgToReplace=reference_pathway)
      
      
      #criate valid edges
      nodes<-current_kgml$nodes[,c('eId', 'eName', 'eReaction')]
      edges<-current_kgml$edges[,c("entry1","entry2")]
      
      edges<-merge(edges,nodes[,c("eId","eName")], by.x="entry1", by.y = "eId")
      edges<-merge(edges,nodes[,c("eId","eName")], by.x="entry2", by.y = "eId")
      
      edges$org<-'ec'
      edges$pathway<-pathway_code
      edges$entry1<-NULL
      edges$entry2<-NULL
      colnames(edges)<-c("node1", "node2", "org", "pathway")
      
      #*************************##
      # Prepare the pathway data #
      #*************************##
      
      # Create the pathwayData dataFrame
      pathwayData <- current_kgml$nodes[,c('eId', 'eName', 'eReaction')]
      
      # Add the default columns
      pathwayData$reaction_type <- NA
      pathwayData$org <- reference_pathway
      pathwayData$pathway <- pathway_code
      pathwayData$is_bottleneck <- 0
      
      pathwayData$freq <- 0
      pathwayData$freq_mean <- 0
      pathwayData$total_species <- 0
      pathwayData$percentage <- 0
      pathwayData$percentage_mean <- 0
      
      pathwayData$betweenness <- NA
      pathwayData$connectivity <- NA
      pathwayData$triangles <- NA
      pathwayData$clusteringCoef <- NA
      pathwayData$closenessCoef <- NA
      pathwayData$community <- NA
      pathwayData$eigenvectorScore <- NA
      pathwayData$eccentricity <- NA
      pathwayData$radius <- NA
      pathwayData$diameter <- NA
      pathwayData$degree <- NA
      pathwayData$authorityScore <- NA
      pathwayData$hubScore <- NA
      
      pathwayData$bottleneck_classification <- NA
      
      # Remove unnecessary data from pathway data/graph
      if (removeNoise_) {
        pathwayData <- removeNoise(pathwayData)
        pathwayGraphOrig <- removeNoise(pathwayGraphOrig)
      }
      
      # Assign the reaction type to each node
      for (idx in 1:nrow(pathwayData)) {
        for (idx2 in 1:length(current_kgml$reactions$name)) {
          # Check the position of the current reaction in reactions list
          if (current_kgml$reactions$name[idx2] %in% pathwayData[idx,]$reaction) {
            pathwayData[idx,]$reaction_type <- current_kgml$reactions$type[idx2]
            break()
          }
        }
      }
      
      # Get the graph properties
      graphProperties<-getGraphProperties(edges)
      graphPropertiesOrig <- getGraphProperties(pathwayGraphOrig)
      
      # Perform the graph bottleneck calculation
      iGraph <- igraph::graph_from_data_frame(edges, directed = TRUE)
      graphBottleneck <- igraph::as_ids(getGraphBottleneck(iGraph, FALSE))
      
      iGraphOrig <- igraph::graph_from_data_frame(pathwayGraphOrig, directed = TRUE)
      graphBottleneckOrig <- igraph::as_ids(getGraphBottleneck(iGraphOrig, FALSE))
      
      # Assign the bottlenecks for enzyme code (ec)
      if (strcmp(reference_pathway, 'ec')) {
        pathwayData$is_bottleneck[which(pathwayData$name %in% graphBottleneck)] <- 1
      }
      
    })
  #}
}

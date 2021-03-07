#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#*************************************************
# Updated in 07/03/2021
#*************************************************

# Start ----

#Clean all variables ----
rm(list=ls(all=TRUE))

# Define base dir ----
#Did you change it to your base location?
dirBase<-"Place here the correct name of your work folder"

dirBase<<-"/media/igorbrandao/IGOR BACKUP SERVER/Arquivos Igor/Faculdades/UFRN/4 - Mestrado/Pesquisas/System biology approaches in the investigation of bottlenecks in KEGG pathways/KeggPathwayAPs"

# bin dir
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

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Functions ----

#*******************************************************#
# Dynamic graph section #
#*******************************************************#

showDynamicGraph<-function(pathway_, org_ = 'ec', auxInfo_ = T, label_ = 'enzyme', removeFake_ = T, dynamicNetwork_ = T) {
  if (!label_ %in% c('enzyme','reaction','id')) {
    stop('Label must be "enzyme", "reaction" or "id".')
  }
  
  # Color pallet
  pal <- brewer.pal(9, "YlOrRd")
  pal2 <- brewer.pal(8, "Dark2")
  
  # Retrieve the pathway ID
  pId = getPathId(paste0(org_, pathway_))
  
  # Retrieve the graph
  lGraph <- getGraphFromPath(pathway = paste0(org_, pathway_), removeFake = removeFake_, auxInfo = auxInfo_)
  
  g1<-lGraph[[1]]
  g4 <-lGraph[[2]]
  
  # Retrieve the network metrics
  networkProperties <- getNodeMetrics(V(g4)$nId, pId)
  
  # Get the iGraph object
  iGraph <- g4
  
  # Convert the iGraph object toVisNetworkData
  data <- toVisNetworkData(iGraph)
  
  if (label_ == 'enzyme') {
    edge_attr(g1,'label')<-edge_attr(g1,"eName")
    vertex_attr(g4,'name')<-vertex_attr(g4,"eName")
  } else if(label_ == 'reaction') {
    edge_attr(g1,'label')<-edge_attr(g1,"rName")
    vertex_attr(g4,'name')<-vertex_attr(g4,"rName")
  }
  
  # Create vis object
  vis.nodes <- networkProperties
  vis.links <- data$edges
  
  # Get the nodes name
  vis.nodes$name = ''

  for (idx in 1:nrow(vis.nodes)) {
    for (idx2 in 1:length(V(g4))) {
      if (vis.nodes[idx,]$nId == V(g4)[idx2]$nId) {
        vis.nodes[idx,]$name = V(g4)[idx2]$name
        break
      }
    }
  }
  
  # Calculates the nodes frequency
  vis.nodes$freq = 0;
  vis.nodes$totalOrg = 0
  vis.nodes$percentage = 0;
  vis.nodes$nPercent = 0;
  
  # Get the total orgs for the current pathway
  totalOrg <- getTotalOrgs(pId)
  vis.nodes$totalOrg = totalOrg
  
  # Get the frequency for each pathway
  for (idx in 1:nrow(vis.nodes)) {
    vis.nodes[idx,]$freq = countNodeFrequency(vis.nodes[idx,]$nId, pId)
    vis.nodes[idx,]$percentage = (vis.nodes[idx,]$freq / totalOrg) * 100
  }
  
  #**************************************************************************##
  # Apply a normalization:                                                    #
  # 100% of occurrence in a pathway can be compared with 50% of other pathway #
  #**************************************************************************##
  
  # Max frequency in a pathway
  pathwayMaxFrequency <- max(vis.nodes$percentage)
  
  # Min frequency in a pathway
  pathwayMinFrequency <- min(vis.nodes$percentage)
  
  # Normalized frequency for each protein
  vis.nodes$nPercent = (vis.nodes$percentage - pathwayMinFrequency) /
    (pathwayMaxFrequency - pathwayMinFrequency) * 100
  
  #**************************************************************************##
  
  # Define the nodes group
  vis.nodes$group = vis.nodes$isAP
  
  if (sum(vis.nodes$group == 1, na.rm = T) > 0) {
    vis.nodes[vis.nodes$group == 1,]$group <- 'AP'
  }
  
  if (sum(vis.nodes$group == 0, na.rm = T) > 0) {
    vis.nodes[vis.nodes$group == 0,]$group <- 'Non-AP'
  }
  
  # Set the initial nodes attributes
  vis.nodes$color.border <- "white"
  vis.nodes$borderWidth <- 0
  
  # Apply the border color by bottleneck status
  vis.nodes$color.border[which(vis.nodes$isAP == 0)] <- "white"
  vis.nodes$color.border[which(vis.nodes$isAP == 1)] <- "blue"
  vis.nodes$borderWidth[which(vis.nodes$isAP == 0)] <- 2 # Node border width
  vis.nodes$borderWidth[which(vis.nodes$isAP == 1)] <- 4 # AP Node border width
  
  vis.nodes$id     <- vis.nodes$nId # Node ID
  vis.nodes$label  <- vis.nodes$name # Node label
  #vis.nodes$label  <- paste0(vis.nodes$name, "\n(", vis.nodes$AP_classification, ")") # Node label
  vis.nodes$title  <- paste0("EC: ", vis.nodes$name, "<br>",
                             #"Entrez: ", vis.nodes$entrez, "<hr>",
                             #"Classification: ", vis.nodes$AP_classification, "<br>",
                             "Is AP: ", ifelse(vis.nodes$isAP==1, 'Yes', 'No') , "<br>",
                             "AP impact: ", vis.nodes$bottleneckImpact, "<br>",
                             "Disconnected components: ", vis.nodes$bottleneckDisconnectedComponents, "<hr>",
                             "Community: ", vis.nodes$community, "<br>",
                             "Degree: ", vis.nodes$degree, "<br>",
                             "Betweenness: ", format(round(vis.nodes$betweenness, 4), nsmall = 4), "<br>",
                             "Clustering coefficient: ", format(round(vis.nodes$clusteringCoef, 4), nsmall = 4), "<br>",
                             "Closeness coefficient: ", format(round(vis.nodes$closenessCoef, 4), nsmall = 4), "<br>",
                             "Authority score: ", vis.nodes$authorityScore, "<br>",
                             "Hub score: ", vis.nodes$hubScore, "<hr>",
                             "Frequency: ", format(round(vis.nodes$percentage, 2), nsmall = 2), "% <hr>",
                             "More info: ", vis.nodes$link) # Text on click
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  
  # Properties when node highlighted
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  vis.nodes$color.highlight.background[which(vis.nodes$isAP == 1)] <- "#20639B"
  vis.nodes$color.highlight.border[which(vis.nodes$isAP == 1)] <- "#173F5F"
  
  betweennessScaleValues <- 1
  
  # Replace NA values
  if (sum(is.na(vis.nodes$betweenness), na.rm = T) > 0) {
    vis.nodes[is.na(vis.nodes$betweenness),]$betweenness = 0
  }
  
  tryCatch({
    # Generates the background color scale
    betweennessScaleValues <- cut(vis.nodes$betweenness, breaks = seq(min(vis.nodes$betweenness),
                                                                      max(vis.nodes$betweenness), len = 100),
                                  include.lowest = TRUE)
    
  }, error=function(e) {})
  
  # Apply the background color scale
  vis.nodes$color.background <- colorRampPalette(pal)(99)[betweennessScaleValues]
  
  # Apply node size according to its frequency
  vis.nodes$size <- scales::rescale(vis.nodes$percentage, to=c(10, 30))
  
  # Set network links properties
  vis.links$width <- 1 # line width
  vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
  vis.links$smooth <- TRUE    # should the edges be curved?
  vis.links$shadow <- FALSE    # edge shadow
  
  # line color
  vis.links$color <- "gray"
  
  if (sum(is.na(vis.links$reaction1Status), na.rm = T) > 0) {
    vis.links[is.na(vis.links$reaction1Status),]$reaction1Status = 'reversible'
  }
  
  if (sum(vis.links$reaction1Status == 'reversible', na.rm = T) > 0) {
    vis.links[vis.links$reaction1Status == 'reversible',]$edge_color <- "gray"
  }
  
  if (sum(vis.links$reaction1Status == 'irreversible', na.rm = T) > 0) {
    vis.links[vis.links$reaction1Status == 'irreversible',]$edge_color <- "darkred"
  }
  
  # Conver the edge nodes Id to name
  vis.links$nodeFrom = ''
  vis.links$nodeTo = ''
  
  for (idx in 1:nrow(vis.links)) {
    vis.links[idx,]$nodeFrom = vis.nodes[vis.nodes$nId == vis.links[idx,]$from,]$name
    vis.links[idx,]$nodeTo = vis.nodes[vis.nodes$nId == vis.links[idx,]$to,]$name
  }
  
  # Line title
  vis.links$title <- paste0(#"Reaction: ", vis.links$reaction1, "<br>",
                            #"Status: ", vis.links$reaction1Status, "<br>",
                            "Node1: ", vis.links$nodeFrom, "<br>",
                            "Node2: ", vis.links$nodeTo, "<br>") # Text on click
  
  # Generate the visNetwor object
  visNetworkObj <- visNetwork(nodes = vis.nodes, edges = vis.links,
                              background="#ffffff", width = '100%', height = '90vh',
                              main=paste0("Pathway ", org_, pathway_),
                              submain=paste0("<b>Nodes:</b> ", length(V(iGraph)), " <b>Edges:</b> ", length(E(iGraph))))
  
  # Generate a dynamic network
  if (dynamicNetwork_) {
    # Add custom physics
    visNetworkObj <- visPhysics(visNetworkObj, stabilization = TRUE, solver = 'forceAtlas2Based',
                                forceAtlas2Based = list(gravitationalConstant = -75, avoidOverlap = 0.3))
    
    # Add custom options
    visNetworkObj <- visOptions(visNetworkObj, autoResize = TRUE, manipulation = TRUE, selectedBy = 'isAP',
                                highlightNearest = list(enabled = T, degree = 2, hover = T))
    
    # Add interaction
    visNetworkObj <- visInteraction(visNetworkObj, navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                                    keyboard = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay = 0)
  } else {
    # Static network
    visNetworkObj <- visPhysics(visNetworkObj, stabilization = TRUE, solver = 'forceAtlas2Based',
                                forceAtlas2Based = list(gravitationalConstant = -75, avoidOverlap = 0.3))
  }
  
  # Generate the network
  return(visNetworkObj)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Pipeline ----
pathway_code = "00010"

# Generate the dynamic network
generatedNetwork <- showDynamicGraph(pathway_ = pathway_code, org_ = 'ec', auxInfo_ = T, label_ = "enzyme", 
                                     removeFake_ = T)

# Export the network
if (!dir.exists(file.path(paste0('./../../output/')))) {
  dir.create(file.path(paste0('./../../output/')), showWarnings = FALSE, mode = "0775")
}

if (!dir.exists(file.path(paste0('./../../output/network/')))) {
  dir.create(file.path(paste0('./../../output/network/')), showWarnings = FALSE, mode = "0775")
}

if (dir.exists(file.path(paste0('./../../output/network/')))) {
  filename <- paste0(pathway_code, '.html')
  
  # Save the HTML file
  visSave(generatedNetwork, file = filename, selfcontained = TRUE)
  
  if (file.exists(filename)) {
    # Copy the file into correct directory
    file.copy(filename, paste0('./../../output/network/', filename), overwrite = TRUE)
    
    # Remove the generated file
    file.remove(filename)
  } else {
    printMessage(paste0("Network file not found. Skipping it..."))
    return(FALSE)
  }
}

# End ----
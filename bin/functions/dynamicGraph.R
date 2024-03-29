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

# Functions ----

#*******************************************************#
# Dynamic graph section #
#*******************************************************#

getGraph<-function(pathway_, 
                   org_) {

  # Retrieve the pathway ID (check here, just work with ec)
  pId = getPathId(paste0("ec", pathway_))
  
  # Retrieve the graph (check here, just work with ec)
  if (org_ == "ec") {
    lGraph <- getGraphFromPath(pathway = paste0("ec", pathway_))
  } else {
    lGraph <- getGraphFromPath(pathway = paste0("ec", pathway_), org = org_)
  }
  
  if (is.null(lGraph)) {
    return(NULL)
  }
  
  g1<-lGraph[[1]]
  g4 <-lGraph[[2]]
  return(g4)
}

getDynamicGraph<-function(g4, 
                          pathway_, 
                          org_,
                          auxInfo_ = T, 
                          label_ = 'enzyme', 
                          removeFake_ = T, 
                          dynamicNetwork_ = T,
                          edgeLength = 'long'){
 
    # Retrieve the pathway ID (check here, just work with ec)
    pId = getPathId(paste0("ec", pathway_))
    if (!label_ %in% c('enzyme','reaction','id')) {
      stop('Label must be "enzyme", "reaction" or "id".')
    }
  if(!edgeLength%in% c('long','medium','short')){
    print('edgeLength must be "long", "medium" or "short". Using "long".')
    edgeLength = 'long'
  }
  if(edgeLength == 'long'){
    edgeLength = -75
  }else  if(edgeLength == 'medium'){
    edgeLength = -35
  } else if(edgeLength == 'short'){
    edgeLength = -15
  }
  # Color pallet
  pal <- brewer.pal(9, "YlOrRd")
  pal2 <- brewer.pal(8, "Dark2")
  
  
  # Retrieve the network metrics
  # networkProperties <- getNodeMetrics(V(g4)$nId, pId)
  networkProperties <- getProperties(g4, pId)
  names(networkProperties)[names(networkProperties) == 'node'] <- 'nId'
  # Get the iGraph object
  iGraph <- g4
  
  # Convert the iGraph object toVisNetworkData
  data <- toVisNetworkData(iGraph)
  
  if (label_ == 'enzyme') {
    # edge_attr(g1,'label')<-edge_attr(g1,"eName")
    vertex_attr(g4,'name')<-vertex_attr(g4,"eName")
  } else if(label_ == 'reaction') {
    # edge_attr(g1,'label')<-edge_attr(g1,"rName")
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
  vis.nodes$associatedEnzymes = "";
  vis.nodes$associatedReactions = "";
  
  # Get the total orgs for the current pathway
  totalOrg <- getTotalOrgs(pId)
  vis.nodes$totalOrg = totalOrg
  
  # Get the frequency for each pathway
  idx=1
  for (idx in 1:nrow(vis.nodes)) {
    vis.nodes[idx,]$freq = countNodeFrequency(vis.nodes[idx,]$nId, pId)
    vis.nodes[idx,]$percentage = (vis.nodes[idx,]$freq / totalOrg) * 100
    
    # Get the nodes associated enzymes and reactions
    createDbConnection()
    associatedEnzymes <- getAssociatedEnzymes(vis.nodes[idx,]$name, pId, org_)
    associatedReactions <- getAssociatedReactions(vis.nodes[idx,]$name, paste0("ec", pathway_))
    
    if (!is.null(associatedEnzymes) && length(associatedEnzymes) != 0) {
      tempEnzymes = ""
      idx2=1
      for (idx2 in 1:nrow(associatedEnzymes)) {
        tempEnzymes <- paste0(tempEnzymes, "<br>", "<a href = ",
                              associatedEnzymes[idx2,]$link, " title='Click me' target=_blank>",
                              associatedEnzymes[idx2,]$enzymeName,'</a>')
      }
      vis.nodes[idx,]$associatedEnzymes <- tempEnzymes
    }
    
    if (!is.null(associatedReactions) && length(associatedReactions) != 0) {
      tempReactions = ""
      for (idx2 in 1:nrow(associatedReactions)) {
        tempReactions <- paste0(tempReactions, "<br>", associatedReactions[idx2,]$rName)
      }
      vis.nodes[idx,]$associatedReactions <- tempReactions
    }
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
  
  #default texts:
  tFrequency ='The frequency with which this gReaction appears in the organisms in KEGG.'
  tDegree='The number of edges that are incident to this vertex.'
  tBetweenness='Measures the extent to which this node lies on paths between other nodes'
  tClustering ='Measures how this node tends to cluster with other nodes.'
  tCloseness='Shows the reciprocal of all shortest paths between this node and all other nodes.'
  tHubScore='Measures how many good authorities this node points for.'
  
  aHref="<a href=# class=tooltip-test style='color:#444444; background-color:#d4d3cd;'title='"
  
  
  vis.nodes$id     <- vis.nodes$nId # Node ID
  vis.nodes$label  <- sub(pattern = 'ec:',
                          replacement ='',
                          x = vis.nodes$name) # Node label
  #vis.nodes$label  <- paste0(vis.nodes$name, "\n(", vis.nodes$AP_classification, ")") # Node label
  vis.nodes$title  <- paste0("gReaction: ", sub(pattern = 'ec:',
                                                replacement ='',
                                                x = vis.nodes$name), "<br>",
                             #"Entrez: ", vis.nodes$entrez, "<hr>",
                             #"Classification: ", vis.nodes$AP_classification, "<br>",
                             "Is AP: ", ifelse(vis.nodes$isAP==1, 'Yes', 'No') , "<br>",
                             "AP impact: ", vis.nodes$bottleneckImpact, "<br>",
                             "Disconnected components: ", vis.nodes$bottleneckDisconnectedComponents, "<hr>",
                             "Community: ", vis.nodes$community, "<br>",
                             paste0(aHref,
                                    tDegree,"' >Degree</a>: "), 
                             vis.nodes$degree, "<br>",
                             paste0(aHref,
                                    tBetweenness,"' >Betweenness</a>: "), 
                             format(round(vis.nodes$betweenness, 4), nsmall = 4), "<br>",
                             paste0(aHref,
                                    tClustering,"' >Clustering coefficient</a>: "), 
                             format(round(vis.nodes$clusteringCoef, 4), nsmall = 4), "<br>",
                             paste0(aHref,
                                    tCloseness,"' >Closeness coefficient</a>: "), 
                             format(round(vis.nodes$closenessCoef, 4), nsmall = 4), "<br>",
                             "Authority score: ", vis.nodes$authorityScore, "<br>",
                             paste0(aHref,
                                    tHubScore,"' >Hub score</a>: "), 
                             vis.nodes$hubScore, "<hr>",
                             paste0(aHref,tFrequency,"' >Frequency</a>: "), format(round(vis.nodes$percentage, 2), nsmall = 2), "% <hr>",
                             "Associated enzymes: ", vis.nodes$associatedEnzymes, "<hr>",
                             "Associated reactions: ", vis.nodes$associatedReactions, "<hr>",
                             "More info: ", vis.nodes$link) # Text on click,
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
    currentNodeFrom <- vis.nodes[vis.nodes$nId == vis.links[idx,]$from,]$name
    currentNodeTo <- vis.nodes[vis.nodes$nId == vis.links[idx,]$to,]$name
    
    if (length(currentNodeFrom) != 0) {
      vis.links[idx,]$nodeFrom <- currentNodeFrom
    } else {
      vis.links[idx,]$nodeFrom <- data$nodes[data$nodes$nId == vis.links[idx,]$from,]$eName
    }
    
    if (length(currentNodeTo) != 0) {
      vis.links[idx,]$nodeTo <- currentNodeTo
    } else {
      vis.links[idx,]$nodeTo <- data$nodes[data$nodes$nId == vis.links[idx,]$to,]$eName
    }
  }
  
  # Line title
  # Box with edges are removed (17/02/2021)
  # vis.links$title <- paste0(#"Reaction: ", vis.links$reaction1, "<br>",
  #                           #"Status: ", vis.links$reaction1Status, "<br>",
  #                           "Node1: ", vis.links$nodeFrom, "<br>",
  #                           "Node2: ", vis.links$nodeTo, "<br>") # Text on click
  
  # Generate the visNetwor object
  visNetworkObj <- visNetwork(nodes = vis.nodes, edges = vis.links,
                              background="#ffffff", width = '100%', height = '90vh',
                              main=paste0("Pathway ", org_, pathway_),
                              submain=paste0("<b>Nodes:</b> ", length(V(iGraph)), " <b>Edges:</b> ", length(E(iGraph))))
  
  # Generate a dynamic network
  if (dynamicNetwork_) {
    # Add custom physics
    visNetworkObj <- visPhysics(visNetworkObj, stabilization = TRUE, solver = 'forceAtlas2Based',
                                forceAtlas2Based = list(gravitationalConstant = edgeLength, avoidOverlap = 0.3))
    
    # Add custom options
    visNetworkObj <- visOptions(visNetworkObj, autoResize = TRUE, manipulation = TRUE, selectedBy = 'isAP',
                                highlightNearest = list(enabled = T, degree = 2, hover = T))
    
    # Add interaction
    visNetworkObj <- visInteraction(visNetworkObj, navigationButtons = FALSE, dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                                    keyboard = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay = 0)
  } else {
    # Static network
    visNetworkObj <- visPhysics(visNetworkObj, stabilization = TRUE, solver = 'forceAtlas2Based',
                                forceAtlas2Based = list(gravitationalConstant = edgeLength, avoidOverlap = 0.3))
  }
  #static positions
  visNetworkObj <- visLayout(visNetworkObj, randomSeed = 0)
  #just order nodes by nId
  visNetworkObj$x$nodes$nId <- as.numeric(visNetworkObj$x$nodes$nId)
  visNetworkObj$x$nodes <- visNetworkObj$x$nodes[order(visNetworkObj$x$nodes$nId),]

  # Generate the network
  return(visNetworkObj)
}

exportNetwork <- function(generatedNetwork_, pathway_code_, org_) {
  # Export the network
  if (!dir.exists(file.path(paste0(dirBase, '/output/')))) {
    dir.create(file.path(paste0(dirBase, '/output/')), showWarnings = FALSE, mode = "0775")
  }
  
  if (!dir.exists(file.path(paste0(dirBase, '/output/networks/')))) {
    dir.create(file.path(paste0(dirBase, '/output/networks/')), showWarnings = FALSE, mode = "0775")
  }
  
  if (!dir.exists(file.path(paste0(dirBase, '/output/networks/', org_)))) {
    dir.create(file.path(paste0(dirBase, '/output/networks/', org_)), showWarnings = FALSE, mode = "0775")
  }
  
  if (dir.exists(file.path(paste0(dirBase, '/output/networks/', org_)))) {
    filename <- paste0(pathway_code_, '.html')
    
    # Save the HTML file
    visSave(generatedNetwork_, file = filename, selfcontained = TRUE)
    
    if (file.exists(filename)) {
      # Copy the file into correct directory
      file.copy(filename, paste0(dirBase, '/output/networks/', org_, '/', filename), overwrite = TRUE)
      
      # Remove the generated file
      file.remove(filename)
      
      printMessage(paste0("Network ", org_, pathway_code_, " generated successfuly!"))
    } else {
      printMessage(paste0("Network file not found. Skipping it..."))
      return(FALSE)
    }
  }
}

fillPathwayCodeWithZeros <- function(dataSet_, verbose_ = TRUE) {
  # Status message
  if (verbose_) {
    printMessage(paste0("FILLING THE PATHWAYS CODE WITH ZEROS..."))
  }
  
  # Count the dataSet rows
  rows <- nrow(dataSet_)
  
  # Loop over the reference pathway rows
  for (idx in 1:rows) {
    current_pathway <- as.integer(dataSet_[idx,]$code)
    
    if (current_pathway >= 1000) {
      dataSet_[idx,]$code <- paste0('0', dataSet_[idx,]$code)
    } else if (current_pathway >= 100 & current_pathway < 1000) {
      dataSet_[idx,]$code <- paste0('00', dataSet_[idx,]$code)
    } else {
      dataSet_[idx,]$code <- paste0('000', dataSet_[idx,]$code)
    }
  }
  
  # Return the result
  return(dataSet_)
}

getProperties <- function(g, pId){
  gProp<-getGraphProperties(g)
  gProp$isAP<-0
  APs<-as.character(getGraphBottleneck(g))
  if(any(!APs %in% gProp$node)){
    stop('At least one AP is missing in ',pathway)
  }
  gProp$pid<-pId
  gProp$isAP[gProp$node %in% APs] <- 1
  
  #for cases where no metric is avaliable
  gProp[is.na(gProp)]<- -1
  
  return(gProp)

}

hideNodes <- function(base,
                      graph,
                      org){
  nada <- alpha(colour = 'white', alpha = 0)
  copy <- base
  idx = 1
  for(idx in 1:nrow(graph$x$nodes)){
    copy$x$nodes[copy$x$nodes$nId == graph$x$nodes$nId[idx],
         c(2:34)] <- graph$x$nodes[idx,c(2:34)]
  }
  
  nodes <- base$x$nodes$nId[!base$x$nodes$nId %in% 
                              graph$x$nodes$nId]
  copy$x$nodes$color.border[base$x$nodes$nId %in% 
                              nodes] <- nada
  copy$x$nodes$color.highlight.background[base$x$nodes$nId %in% 
                                            nodes] <- nada
  copy$x$nodes$color.highlight.border[base$x$nodes$nId %in% 
                                        nodes] <- nada
  copy$x$nodes$color.background[base$x$nodes$nId %in% 
                                  nodes] <- nada
  copy$x$nodes$size[base$x$nodes$nId %in% 
                                  nodes] <- 0
  copy$x$nodes$size[base$x$nodes$nId %in% 
                      nodes] <- 0
  copy$x$nodes$label[base$x$nodes$nId %in% 
                      nodes] <- ''
  copy$x$edges$color[base$x$edges$from %in% 
                       nodes] <- nada
  copy$x$edges$color[base$x$edges$to %in% 
                       nodes] <- nada
  copy$x$main$text<- sub(x=copy$x$main$text,
                         pattern = "ec", 
                         replacement = org)
  edges <- nrow(copy$x$edges[copy$x$edges$color!= nada,])
  nodes <- nrow(copy$x$nodes[copy$x$nodes$color.background!= nada,])
  edgesOld <- nrow(base$x$edges)
  nodesOld <- nrow(base$x$nodes)
  
  copy$x$submain$text<- sub(x=copy$x$submain$text,
                            pattern = nodesOld, 
                            replacement = nodes)
  copy$x$submain$text<- sub(x=copy$x$submain$text,
                            pattern = edgesOld,
                            replacement = edges)
  return(copy)
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# End ----
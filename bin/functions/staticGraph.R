#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#*************************************************
# Updated in 24/05/2022
#*************************************************

# Functions ----

#*******************************************************#
# Dynamic graph section #
#*******************************************************#

#' Function to generate a static network based on ggraph library
#'
#' @param network_ Network data frame with all properties.
#' @param networkProperties_ Contains main information about the network nodes.
#' @param pathway_ Network name.
#' @param org_ Organism name.
#'
#' @return This function does not return nothing, just export files.
#'
#' @examples
#' \dontrun{
#' generateStaticNetwork(pathwayData, properties)
#' }
#'
#' @author
#' Igor Brandão
#'
#'

# Fake data for test purpose....
showStaticGraph<-function(pathway_, org_, auxInfo_ = T, label_ = 'enzyme', removeFake_ = T, customLayout_="gem") {
  #----------------------------#
  # [RETRIEVE GRAPH DATA] #
  #----------------------------#
  if (!label_ %in% c('enzyme','reaction','id')) {
    stop('Label must be "enzyme", "reaction" or "id".')
  }
  
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
  
  if (label_ == 'enzyme') {
    edge_attr(g1,'label')<-edge_attr(g1,"eName")
    vertex_attr(g4,'name')<-vertex_attr(g4,"eName")
  } else if(label_ == 'reaction') {
    edge_attr(g1,'label')<-edge_attr(g1,"rName")
    vertex_attr(g4,'name')<-vertex_attr(g4,"rName")
  }
  
  # Retrieve the network metrics
  networkProperties <- getNodeMetrics(V(g4)$nId, pId)
  
  # Get the iGraph object
  iGraph <- g4
  
  #----------------------------#
  # [ORGANIZZE THE GRAPH DATA] #
  #----------------------------#
  vertices <- networkProperties
  relations <- as.data.frame(get.edgelist(iGraph))
  
  #--------------------------#
  # [PREPARE THE GRAPH DATA] #
  #--------------------------#

  # Adjust the columns names
  names(relations)[names(relations) == "V1"] <- "from"
  names(relations)[names(relations) == "V2"] <- "to"

  # Calculates the nodes frequency
  vertices$freq = 0;
  vertices$totalOrg = 0
  vertices$percentage = 0;
  vertices$nPercent = 0;
  vertices$associatedEnzymes = "";
  vertices$associatedReactions = "";
  
  # Get the total orgs for the current pathway
  totalOrg <- getTotalOrgs(pId)
  vertices$totalOrg = totalOrg
  
  # Get the frequency for each pathway
  for (idx in 1:nrow(vertices)) {
    vertices[idx,]$freq = countNodeFrequency(vertices[idx,]$nId, pId)
    vertices[idx,]$percentage = (vertices[idx,]$freq / totalOrg) * 100
    
    # Get the nodes associated enzymes and reactions
    createDbConnection()
    associatedEnzymes <- getAssociatedEnzymes(vertices[idx,]$name, paste0("ec", pathway_))
    associatedReactions <- getAssociatedReactions(vertices[idx,]$name, paste0("ec", pathway_))
    
    if (!is.null(associatedEnzymes) && length(associatedEnzymes) != 0) {
      tempEnzymes = ""
      for (idx2 in 1:nrow(associatedEnzymes)) {
        tempEnzymes <- paste0(tempEnzymes, "<br>", associatedEnzymes[idx2,]$enzymeName)
      }
      vertices[idx,]$associatedEnzymes <- tempEnzymes
    }
    
    if (!is.null(associatedReactions) && length(associatedReactions) != 0) {
      tempReactions = ""
      for (idx2 in 1:nrow(associatedReactions)) {
        tempReactions <- paste0(tempReactions, "<br>", associatedReactions[idx2,]$rName)
      }
      vertices[idx,]$associatedReactions <- tempReactions
    }
  }
  
  #**************************************************************************##
  # Apply a normalization:                                                    #
  # 100% of occurrence in a pathway can be compared with 50% of other pathway #
  #**************************************************************************##
  
  # Max frequency in a pathway
  pathwayMaxFrequency <- max(vertices$percentage)
  
  # Min frequency in a pathway
  pathwayMinFrequency <- min(vertices$percentage)
  
  # Normalized frequency for each protein
  vertices$nPercent = (vertices$percentage - pathwayMinFrequency) /
    (pathwayMaxFrequency - pathwayMinFrequency) * 100
  
  #**************************************************************************##

  #---------------------#
  # [VERTEX AESTHETICS] #
  #---------------------#
  
  # Get the nodes name
  vertices$name = ''
  
  for (idx in 1:nrow(vertices)) {
    for (idx2 in 1:length(V(g4))) {
      if (vertices[idx,]$nId == V(g4)[idx2]$nId) {
        vertices[idx,]$name = V(g4)[idx2]$name
        break
      }
    }
  }

  # Set the node label
  vertices$label <- vertices$name

  # Set the initial nodes aesthetic attributes
  vertices$color.border <- "white"
  vertices$borderWidth <- 0
  vertices$AP_classification <- ""

  # Apply the border color by bottleneck status
  if (sum(vertices$isAP == 0, na.rm = T) > 0) {
    vertices$color.border[which(vertices$isAP == 0)] <- "#ffffff"
    vertices$borderWidth[which(vertices$isAP == 0)] <- 1 # Node border width
    vertices$AP_classification[which(vertices$isAP == 0)] <- "Non-AP"
  }

  if (sum(vertices$isAP == 1, na.rm = T) > 0) {
    vertices$color.border[which(vertices$isAP == 1)] <- "#005b96"
    vertices$borderWidth[which(vertices$isAP == 1)] <- 2 # AP Node border width
    vertices$AP_classification[which(vertices$isAP == 1)] <- "AP"
  }

  # Vertex drop shadow
  vertices$shadow <- TRUE # Nodes will drop shadow

  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # ********************************************** #
  # Vertex aesthetics bound to scalar attributes
  # ********************************************** #

  # Vertex background color scale according to the betweenness
  betweennessScaleValues <- 1

  # Replace NA values
  if (sum(is.na(vertices$betweenness), na.rm = T) > 0) {
    vertices[is.na(vertices$betweenness),]$betweenness = 0
  }

  tryCatch({
    # Generates the background color scale
    betweennessScaleValues <- cut(vertices$betweenness, breaks = seq(min(vertices$betweenness),
        max(vertices$betweenness), len = 100), include.lowest = TRUE)

  }, error=function(e) {})

  # Color pallet
  pal <- brewer.pal(9, "YlOrBr")

  # Apply the background color scale
  vertices$color.background <- colorRampPalette(pal)(99)[betweennessScaleValues]

  # Apply node size according to its frequency
  vertices$vertex_size <- scales::rescale(vertices$percentage, to=c(0, 100))

  #--------------------#
  # [EDGES AESTHETICS] #
  #--------------------#

  # Set network links properties
  relations$edge_width <- 0.1 # line width
  relations$edge_arrows <- "middle" # arrows: 'from', 'to', or 'middle'
  relations$edge_smooth <- TRUE    # should the edges be curved?
  relations$edge_shadow <- FALSE    # edge shadow
  
  # line color
  relations$edge_color <- "gray"
  
  if (sum(is.na(relations$reaction1Status), na.rm = T) > 0) {
    relations[is.na(relations$reaction1Status),]$reaction1Status = 'reversible'
  }
  
  if (sum(relations$reaction1Status == 'reversible', na.rm = T) > 0) {
    relations[relations$reaction1Status == 'reversible',]$edge_color <- "gray"
  }
  
  if (sum(relations$reaction1Status == 'irreversible', na.rm = T) > 0) {
    relations[relations$reaction1Status == 'irreversible',]$edge_color <- "darkred"
  }

  #-------------------------#
  # [SET THE FACTORS ORDER] #
  #-------------------------#

  # Order the vertices by the name
  #vertices$nId <- factor(vertices$nId, levels = vertices$nId[order(vertices$nId)])
  #relations$from <- factor(relations$from, levels = vertices$nId[order(vertices$nId)])

  # Reorder the vertice dataframe to make sure the graph_from_data_frame will use name instead of nId
  vertices <-vertices %>% relocate(name, .before = nId)
  
  #-----------------------------#
  # [GENERATE THE GRAPH OBJECT] #
  #-----------------------------#
  iGraph <- igraph::graph_from_data_frame(relations, directed = FALSE, vertices = vertices)

  #------------------#
  # [PLOT THE GRAPH] #
  #------------------#
  options(ggrepel.max.overlaps = Inf)
  
  # Validate the used layout since the sparse stress require additional parameters
  if (customLayout_ == "sparse_stress") {
    layout <- create_layout(iGraph, layout = customLayout_, pivots = nrow(vertices), weights = NA)
  } else if (customLayout_ == "centrality") {
    layout <- create_layout(iGraph, layout = customLayout_, centrality = betweenness)
  } else {
    layout <- create_layout(iGraph, layout = customLayout_)
  }
  
  staticGraph <- ggraph(layout)
  staticGraph <- staticGraph +
    # Edges
    geom_edge_fan(aes(colour = edge_color),
                  strength=2,
                  edge_alpha = 0.5,
                  angle_calc = 'along',
                  label_dodge = unit(2.5, 'mm'),
                  arrow = arrow(length = unit(2, 'mm'), type = 'closed'),
                  end_cap = circle(3, 'mm'),
                  check_overlap = TRUE,
                  show.legend = NA) +

    # Nodes
    geom_node_point(aes(fill = betweenness, size = vertex_size, stroke = borderWidth, colour = AP_classification),
                    shape=21, alpha = 1) +

    # Nodes label
    geom_node_text(aes(filter = vertex_size >= 0, label = label),
                   size = 3, family="serif", repel = TRUE, check_overlap = TRUE,
                   nudge_y = -0.19) +

    # Nodes customization
    scale_fill_gradientn("Betweenness", colours = brewer.pal(9, "YlOrBr"), limits=c(min(vertices$betweenness), max(vertices$betweenness))) +
    scale_color_manual("AP classification", values = c('blue', 'black')) +

    # Edges customization
    scale_edge_color_manual("", values = c('grey66', 'grey66')) +
    scale_edge_width_continuous(range = c(0.2,3)) +

    # Theme
    theme_graph() +
    theme(legend.position = "right") +

    # Rename the legends
    labs(size = "Enzymes frequency (%)") +

    # Set the legends order
    guides(size = guide_legend(order = 1),
           colour = guide_legend(order = 2))

  staticGraph
}

exportNetwork <- function(org_, pathway_) {
  #---------------------------#
  # [EXPORT THE GRAPH FIGURE] #
  #---------------------------#
  filename <- paste0(org_, pathway_)
  
  # Export the network
  if (!dir.exists(file.path(paste0('./output/network/static')))) {
    dir.create(file.path(paste0('./output/network/static')), showWarnings = FALSE, mode = "0775")
  }
  
  if (!dir.exists(file.path(paste0('./output/network/static/', pathway_)))) {
    dir.create(file.path(paste0('./output/network/static/', pathway_)), showWarnings = FALSE, mode = "0775")
  }
  
  if (dir.exists(file.path(paste0('./output/network/static/', pathway_)))) {
    ggsave(paste0('./output/network/static/', pathway_, '/', filename, '.png'), width = 35, height = 20, units = "cm")
    
    # Export the SVG file
    if (!dir.exists(file.path(paste0('./output/network/static/svg')))) {
      dir.create(file.path(paste0('./output/network/static/svg')), showWarnings = FALSE, mode = "0775")
    }
    
    ggsave(paste0('./output/network/static/svg/', filename, '.svg'), width = 35, height = 20, units = "cm")
  }
}

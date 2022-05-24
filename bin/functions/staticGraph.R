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
generateStaticNetwork <- function(network_, networkProperties_, pathway_="", org_="", customLayout_="sparse_stress") {

  #----------------------------#
  # [ORGANIZZE THE GRAPH DATA] #
  #----------------------------#
  vertices <- networkProperties_
  relations <- network_

  #--------------------------#
  # [PREPARE THE GRAPH DATA] #
  #--------------------------#

  # Remove unnecessary data
  vertices$X <- NULL
  vertices$x <- NULL
  vertices$y <- NULL

  # Adjust the columns names
  names(relations)[names(relations) == "node1"] <- "from"
  names(relations)[names(relations) == "node2"] <- "to"

  # Change the relation from -> to names
  relations$from <- relations$entryID1
  relations$to <- relations$entryID2

  #---------------------#
  # [VERTEX AESTHETICS] #
  #---------------------#

  # Set the node label
  vertices$label <- vertices$name

  # Set the initial nodes aesthetic attributes
  vertices$color.border <- "white"
  vertices$borderWidth <- 0

  # Apply the border color by bottleneck status
  if (sum(vertices$is_bottleneck == 0, na.rm = T) > 0) {
    vertices$color.border[which(vertices$is_bottleneck == 0)] <- "#ffffff"
    vertices$borderWidth[which(vertices$is_bottleneck == 0)] <- 1 # Node border width
  }

  if (sum(vertices$is_bottleneck == 1, na.rm = T) > 0) {
    vertices$color.border[which(vertices$is_bottleneck == 1)] <- "#005b96"
    vertices$borderWidth[which(vertices$is_bottleneck == 1)] <- 2 # AP Node border width
  }

  # Vertex drop shadow
  vertices$shadow <- TRUE # Nodes will drop shadow

  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # ********************************************** #
  # Vertex aesthetics binded to scalar attributes
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
  relations$edge_color <- NA

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

  # Order the vertices by the entryID
  vertices$entryID <- factor(vertices$entryID, levels = vertices$entryID[order(vertices$entryID)])
  relations$from <- factor(relations$from, levels = vertices$entryID[order(vertices$entryID)])

  #-----------------------------#
  # [GENERATE THE GRAPH OBJECT] #
  #-----------------------------#
  iGraph <- igraph::graph_from_data_frame(relations, directed = FALSE, vertices = vertices)

  #------------------#
  # [PLOT THE GRAPH] #
  #------------------#
  # Validate the used layout since the sparse stress require additional parameters
  if (customLayout_ == "sparse_stress") {
    staticGraph <- ggraph(iGraph, layout = customLayout_, pivots = nrow(vertices), weights = NA)
  } else {
    staticGraph <- ggraph(iGraph, layout = customLayout_)
  }

  staticGraph <- staticGraph +
    # Edges
    geom_edge_fan(aes(colour = reaction1Status),
                  edge_alpha = 0.5,
                  angle_calc = 'along',
                  label_dodge = unit(2.5, 'mm'),
                  arrow = arrow(length = unit(2, 'mm'), type = 'closed'),
                  end_cap = circle(3, 'mm')) +

    # Nodes
    geom_node_point(aes(fill = betweenness, size = vertex_size, stroke = borderWidth, colour = AP_classification),
                    shape=21, alpha = 1) +

    # Nodes label
    geom_node_text(aes(filter = vertex_size >= 0, label = label),
                   size = 3, family="serif", repel = TRUE, check_overlap = TRUE,
                   nudge_y = -0.19) +

    # Nodes customizations
    scale_fill_gradientn("Betweenness", colours = brewer.pal(9, "YlOrBr"), limits=c(min(vertices$betweenness), max(vertices$betweenness))) +
    scale_color_manual("AP classification", values = c('blue', 'black')) +

    # Edges customizations
    scale_edge_color_manual("Reaction status", values = c('#ff7b7b', 'grey66')) +
    scale_edge_width_continuous(range = c(0.2,3)) +

    # Theming
    theme_graph() +
    theme(legend.position = "right") +

    # Rename the legends
    labs(size = "Enzymes frequency (%)") +

    # Set the legends order
    guides(size = guide_legend(order = 1),
           colour = guide_legend(order = 2))

  staticGraph
}

exportNetwork <- function(filename) {
  #---------------------------#
  # [EXPORT THE GRAPH FIGURE] #
  #---------------------------#
  filename <- paste0(org_, pathway_)
  
  # Export the network
  if (!dir.exists(file.path(paste0('./output/network/static')))) {
    dir.create(file.path(paste0('./output/network/static')), showWarnings = FALSE, mode = "0775")
  }
  
  if (dir.exists(file.path(paste0('./output/network/static')))) {
    ggsave(paste0('./output/network/static/', filename, '.png'), width = 35, height = 20, units = "cm")
    
    # Export the svg
    if (!dir.exists(file.path(paste0('./output/network/static/svg')))) {
      dir.create(file.path(paste0('./output/network/static/svg')), showWarnings = FALSE, mode = "0775")
    }
    
    ggsave(paste0('./output/network/static/svg/', filename, '.svg'), width = 35, height = 20, units = "cm")
  }
}

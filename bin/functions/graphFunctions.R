#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#****************************************#
# Functions to handle graph manipulation #
#****************************************#

# graphFunctions.R #

# ---- IMPORT SECTION ----

#' This is set of functions to handle graphs
#'
#' @author
#' Igor Brandão

# Import the necessary libraries
library(igraph)
library(pracma) # string manipulation

#*******************************************************************************************#

# ---- GRAPH PROPERTIES SECTION ----

#' Get some graph properties from a directed graph
#'
#' Given a data.frame of directed edges, this function computes the connectivity,
#' clustering coefficient, and betweenness.
#'
#' @param iGraph_ A data.frame containing directed edges from a KEGG graph.
#'
#' @return This function returns a data.frame containing three columns: connectivity,
#' clustering coefficient, and betweenness.
#'
#' @examples
#' \dontrun{
#' df <- getGraphProperties(pathwayToDataframe("hsa00010"))
#' }
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph betweenness
#' @importFrom igraph count_triangles
#' @importFrom igraph transitivity
#' @importFrom igraph closeness
#'
#' @author
#' Diego Morais / Igor Brandão

getGraphProperties <- function(g) {

  tryCatch({
    # Convert to dataFrame the iGraph object
    iGraph_<- igraph::as_data_frame(g)
    #g <- igraph::graph_from_data_frame(iGraph_, directed = TRUE)

    # Calculates betweenness
    # Measure of centrality in a graph based on shortest paths
    betweenness <- igraph::betweenness(g, normalized = TRUE)

    # Define the result dataFrame
    result <- data.frame(node = names(betweenness), betweenness = betweenness,
                         stringsAsFactors = FALSE)

    rownames(result) <- NULL

    k <- as.data.frame(table(iGraph_$from))

    # Calculates the connectivity
    # # minimum number of elements (nodes or edges) that need to be removed to separate
    # the remaining nodes into isolated subgraphs.
    result$connectivity <- 0
    result$connectivity <- k[match(result$node, k$Var1), 2]
    result$connectivity[is.na(result$connectivity)] <- 0

    # Calculates the triangles
    # How many triangles a vertex is part of, in a graph, or just list the triangles of a graph.
    result$triangles <- vapply(result$node, function(x){
      as.integer(igraph::count_triangles(g, vids = x))
    }, integer(1))

    # Calculates the clustering coefficient
    # Transitivity measures the probability that the adjacent vertices of a vertex are connected
    result$clusteringCoef <- igraph::transitivity(g, vids = result$node,
                                                  isolates = "zero",
                                                  type = "local")

    # Calculates the closeness coefficient
    # Measures how many steps is required to access every other vertex from a given vertex.
    result$closenessCoef <- suppressWarnings(igraph::closeness(g, vids=result$node))

    # Calculates the vertex communities
    # greedy method (hiearchical, fast method)
    # c3 = suppressWarnings(cluster_edge_betweenness(g, 
    #                               directed = T,
    #                               modularity = F))
    # result$community <- as.integer(membership(c3))
    result$community <- 0
    
    # Calculates the Eigenvector Centrality Scores of Network Positions
    # It is a measure of the influence of a node in a network.
    eigen_centrality <- igraph::eigen_centrality(g, directed = TRUE)
    result$eigenvectorScore <- unlist(eigen_centrality[1]) # Just the scores

    # Calculates the eccentricity of a vertex
    # It is defined as the maximum distance of one vertex from other vertex
    result$eccentricity <- igraph::eccentricity(g, vids=result$node)

    # Calculates the radius of a vertex (entire graph)
    # The smallest eccentricity in a graph is called its radius
    result$radius <- igraph::radius(g)

    # Calculates the diameter of a vertex (entire graph)
    # The diameter of a graph is the length of the longest geodesic
    result$diameter <- igraph::diameter(g, directed = TRUE)

    # Calculates the degree of a vertex
    # The degree of a vertex is the number of adjacent edges
    result$degree <- igraph::degree(g, v=result$node)

    # Calculates the Kleinberg's authority centrality scores
    # The authority scores of the vertices are defined as the principal
    # eigenvector of t(A)*A, where A is the adjacency matrix of the graph
    authority_score <- igraph::authority_score(g)
    result$authorityScore <- unlist(authority_score[1]) # Just the scores

    # Calculates the Kleinberg's hub centrality scores
    # The hub scores of the vertices are defined as the principal eigenvector
    # of A*t(A), where A is the adjacency matrix of the graph
    hub_score <- igraph::hub_score(g)
    result$hubScore <- unlist(hub_score[1]) # Just the scores

    # Return the result data frame
    return(result)

  }, error=function(e) {
    print(paste0('It wasnt possible to retrieve properties from the graph. Skipping it...'))
    return(NULL)
  })
}

getGraphPropertiesOld <- function(iGraph_) {
  
  tryCatch({
    # Convert the dataFrame to iGraph object
    g <- igraph::graph_from_data_frame(iGraph_, directed = TRUE)
    
    # Calculates betweenness
    # Measure of centrality in a graph based on shortest paths
    betweenness <- igraph::betweenness(g, normalized = TRUE)
    
    # Define the result dataFrame
    result <- data.frame(node = names(betweenness), betweenness = betweenness,
                         stringsAsFactors = FALSE)
    
    rownames(result) <- NULL
    
    k <- as.data.frame(table(iGraph_$node1))
    
    # Calculates the connectivity
    # # minimum number of elements (nodes or edges) that need to be removed to separate
    # the remaining nodes into isolated subgraphs.
    result$connectivity <- 0
    result$connectivity <- k[match(result$node, k$Var1), 2]
    result$connectivity[is.na(result$connectivity)] <- 0
    
    # Calculates the triangles
    # How many triangles a vertex is part of, in a graph, or just list the triangles of a graph.
    result$triangles <- vapply(result$node, function(x){
      as.integer(igraph::count_triangles(g, vids = x))
    }, integer(1))
    
    # Calculates the clustering coefficient
    # Transitivity measures the probability that the adjacent vertices of a vertex are connected
    result$clusteringCoef <- igraph::transitivity(g, vids = result$node,
                                                  isolates = "zero",
                                                  type = "local")
    
    # Calculates the closeness coefficient
    # Measures how many steps is required to access every other vertex from a given vertex.
    result$closenessCoef <- suppressWarnings(igraph::closeness(g, vids=result$node))
    
    # Calculates the vertex communities
    # greedy method (hiearchical, fast method)
    c3 = cluster_edge_betweenness(g)
    result$community <- as.integer(membership(c3))
    
    # Calculates the Eigenvector Centrality Scores of Network Positions
    # It is a measure of the influence of a node in a network.
    eigen_centrality <- igraph::eigen_centrality(g, directed = TRUE)
    result$eigenvectorScore <- unlist(eigen_centrality[1]) # Just the scores
    
    # Calculates the eccentricity of a vertex
    # It is defined as the maximum distance of one vertex from other vertex
    result$eccentricity <- igraph::eccentricity(g, vids=result$node)
    
    # Calculates the radius of a vertex (entire graph)
    # The smallest eccentricity in a graph is called its radius
    result$radius <- igraph::radius(g)
    
    # Calculates the diameter of a vertex (entire graph)
    # The diameter of a graph is the length of the longest geodesic
    result$diameter <- igraph::diameter(g, directed = TRUE)
    
    # Calculates the degree of a vertex
    # The degree of a vertex is the number of adjacent edges
    result$degree <- igraph::degree(g, v=result$node)
    
    # Calculates the Kleinberg's authority centrality scores
    # The authority scores of the vertices are defined as the principal
    # eigenvector of t(A)*A, where A is the adjacency matrix of the graph
    authority_score <- igraph::authority_score(g)
    result$authorityScore <- unlist(authority_score[1]) # Just the scores
    
    # Calculates the Kleinberg's hub centrality scores
    # The hub scores of the vertices are defined as the principal eigenvector
    # of A*t(A), where A is the adjacency matrix of the graph
    hub_score <- igraph::hub_score(g)
    result$hubScore <- unlist(hub_score[1]) # Just the scores
    
    # Return the result data frame
    return(result)
    
  }, error=function(e) {
    print(paste0('It wasnt possible to retrieve properties from the graph. Skipping it...'))
    return(NULL)
  })
}


#' Given an iGraph object, this function set each vertex communities
#'
#' @param iGraph_ An iGraph object.
#'
#' @return This function returns the same iGraph object with an extra 'group' column.
#'
#' @examples
#' \dontrun{
#' iGraphObj <- setGraphCommunity(iGraphObj)
#' }
#'
#' @importFrom igraph cluster_edge_betweenness
#' @importFrom igraph membership
#' @importFrom igraph modularity
#'
#' @author
#' Igor Brandão

setGraphCommunity <- function(iGraph_, verbose_=FALSE) {
  # greedy method (hiearchical, fast method)
  #do not work for direted graph
  c3 = cluster_edge_betweenness(iGraph_)

  # define the cluster attribute
  V(iGraph_)$group <- membership(c3)

  # modularity measure
  if (verbose_) {
    print(modularity(c3))
    print("Graph community setted successfully!")
  }

  # return the iGraph object
  return(iGraph_)
}

setGraphBetwenness <- function(iGraph_, normalized_=TRUE, verbose_=FALSE) {
  # calculates the betweenness
  V(iGraph_)$betweenness <- betweenness(iGraph_, normalized = normalized_)

  # normalizes betweenness
  if (normalized_) {
    V(iGraph_)$betweenness <- V(iGraph_)$betweenness/max(V(iGraph_)$betweenness)
  }

  # print the betweenness
  if (verbose_) {
    print(V(iGraph_)$betweenness)
    print("Graph betweenness setted successfully!")
  }

  # return the iGraph object
  return(iGraph_)
}

setGraphCloseness <- function(iGraph_, verbose_=FALSE) {
  # closeness refers to how connected a node is to its neighbors
  V(iGraph_)$closeness <- closeness(iGraph_, vids=V(iGraph_))

  # print the closeness
  if (verbose_) {
    print(V(iGraph_)$closeness)
    print("Graph closeness setted successfully!")
  }

  # return the iGraph object
  return(iGraph_)
}

setGraphClustering <- function(iGraph_, verbose_=FALSE) {
  # calculates the local clustering coefficient for each vertex
  V(iGraph_)$clustering <- transitivity(iGraph_, type = c("local"), vids = NULL,
                                        weights = NULL, isolates = c("NaN", "zero"))

  # print the clustering
  if (verbose_) {
    print(V(iGraph_)$clustering)
    print("Graph clustering setted successfully!")
  }

  # return the iGraph object
  return(iGraph_)
}

getTopBetweenness <- function(iGraph_, betweenness_percentual_rate_=0.2, verbose_=FALSE) {
  topBetweenness <- sort(V(iGraph)$betweenness, decreasing=TRUE)
  topBetweennessWithRate <- topBetweenness[1:as.integer(length(topBetweenness) * betweenness_percentual_rate_)]

  # print the clustering
  if (verbose_) {
    print(V(iGraph)[match(topBetweennessWithRate, V(iGraph)$betweenness)])
    print(topBetweennessWithRate)
  }

  return(topBetweennessWithRate)
}

#*******************************************************************************************#

# ---- BOTTLENECK DETECTION ----

#' Function to calculate the pathway bridges based on brute force
#' Complexity: O(V*(V+E))
#'
#' @param iGraph_ Pathway iGraph object.
#' @param verbose_ Whether or not print the status message
#'
#' @return This function returns a list of bridges (nodes id).
#'
#' @examples
#' \dontrun{
#' getGraphBridges(iGraph, verbose_=TRUE)
#' }
#'
#' @author
#' Igor Brandão

getGraphBridges <- function(iGraph_, verbose_=FALSE) {
  # Define the result vector
  result <- data.frame(bridges = NA)

  # Convert the igraph object into graph
  G <- igraph::graph_from_data_frame(iGraph_, directed = FALSE)

  # Count the number of decomposed graphs
  num_comp <- length(decompose.graph(G))

  for (i in 1:length(E(G))) {
    G_sub <- delete.edges(G, i)
    if (length(decompose.graph(G_sub)) > num_comp) {
      result <- rbind(result, as_ids(E(G)[i]))
    }
  }

  # print the articulation points
  if (verbose_) {
    print(result)
    print("Graph bridges calculated successfully!")
  }

  return(result[complete.cases(result),])
}

#' Function to calculate the pathway bottlenecks
#' Complexity: O(V+E)
#'
#' @param iGraph_ Pathway iGraph object.
#' @param verbose_ Whether or not print the status message
#'
#' @return This function returns a list of articulation points (nodes id).
#'
#' @examples
#' \dontrun{
#' getGraphBottleneck(iGraph)
#' }
#'
#' @author
#' Igor Brandão

getGraphBottleneck <- function(iGraph_, verbose_=FALSE) {
  articulation_points <- igraph::articulation_points(iGraph_)
  APs <- vertex_attr(graph = iGraph_ , 
                     name = "name", 
                     index = articulation_points)
  # print the articulation points
  if (verbose_) {
    print(articulation_points)
    print("Graph bottlenecks calculated successfully!")
  }

#  return(articulation_points)
  return(APs)
}

#' Function to classify the bottlenecks into the following groups:
#'
#' HB - Hub botlenecks
#' NHB - Non Hub bottlenecks
#' HNB - Hub non bottlenecks
#' NHNB - Non hub non bottlenecks
#'
#' @param networkProperties_ Contains main information about the network nodes.
#'
#' @return This function returns the same inputted data frame with an additional column
#'
#' @examples
#' \dontrun{
#' classifyBottleneck(network, properties)
#' pathwayData$bottleneck_classification <- classifyBottleneck(pathwayData)$bottleneck_classification
#' }
#'
#' @author
#' Igor Brandão

classifyBottleneck <- function(networkProperties_) {

  applyClassification <- function(idx_) {
    node <- networkProperties_[idx_,]
    classification <- ''

    #************************************#
    # Step 1: Check if the node is a hub #
    #************************************#

    # Get the top 20% degrees (Yu, Kim et al)
    degree_percentual_rate_ <- 0.2
    topDegrees <- sort(networkProperties_$degree, decreasing=TRUE)
    topDegrees <- topDegrees[1:as.integer(length(topDegrees) * degree_percentual_rate_)]

    if (node$degree %in% topDegrees) {
      classification <- paste0(classification, 'H')
    } else {
      classification <- paste0(classification, 'NH')
    }

    # Step 2: Check if the node is a bottleneck
    if (node$is_bottleneck) {
      classification <- paste0(classification, 'B')
    } else {
      classification <- paste0(classification, 'NB')
    }

    # Return the bottleneck classification
    networkProperties_[idx_,]$bottleneck_classification <<- classification
  }

  # First of all add the classification column
  networkProperties_$bottleneck_classification <- NA

  # Process each node
  sapply(1:nrow(networkProperties_), applyClassification)

  # Return the updated dataframe
  return(networkProperties_)
}

getArticulationPointImpact <- function(graph_, verbose_=FALSE) {

  # Status message
  if (verbose_) {
    print("Calculating the graph articulation points...")
  }

  # Select only the columns containing the nodes
  graph_ <- graph_[,c('node1', 'node2')]

  # Convert the graph into iGraph object
  g <- igraph::graph_from_data_frame(graph_, directed = TRUE)

  # Calculate the articulation points
  articulation_points <- getGraphBottleneck(g)

  # Status message
  if (verbose_) {
    print("Calculating the articulation points impact...")
  }

  if (!is.null(articulation_points) && length(articulation_points) > 0) {
    # Unify the graph nodes and set its community
    result <- data.frame(ap=as_ids(articulation_points), noComponents=0, componentsSize="0", impact=0, weightedImpact=0, stringsAsFactors = FALSE)

    # Dismantle the graph to calculate its impact
    for (idx in 1:length(articulation_points)) {
      # Set the current articulation point
      currentAP <- articulation_points[idx]

      # Set a temp graph without the current articulation point
      tempGraph <- delete_vertices(g, currentAP)

      # Calculate the number and size of the disconnected components
      components <- components(tempGraph)
      result$noComponents[idx] <- components$no
      result$componentsSize[idx] <- paste(components$csize, collapse = ', ')

      # Calculate the AP impact based on: Gabriele Farina paper
      result$impact[idx] <- min(components$csize)

      # Calculate the AP weighted impact from original creation
      result$weightedImpact[idx] <- (min(components$csize) * components$no)

      rm(tempGraph)
    }
  } else {
    result <- NULL
  }

  return(result)
}

getArticulationPointSubGraphs <- function(graph_, verbose_=FALSE) {

  # Status message
  if (verbose_) {
    print("Calculating the graph articulation points...")
  }

  # Select only the columns containing the nodes
  graph_ <- graph_[,c('node1', 'node2')]

  # Convert the graph into iGraph object
  g <- igraph::graph_from_data_frame(graph_, directed = TRUE)

  # Calculate the articulation points
  articulation_points <- getGraphBottleneck(g)

  # Status message
  if (verbose_) {
    print("Calculating the articulation points impact...")
  }

  if (!is.null(articulation_points) && length(articulation_points) > 0) {
    # Unify the graph nodes and set its community
    result <- data.frame(ap=as_ids(articulation_points),
                         eccentricity=igraph::eccentricity(g, articulation_points),
                         degree=igraph::degree(g, articulation_points),
                         closeness=igraph::closeness(g, articulation_points),
                         betweenness=igraph::betweenness(g, articulation_points, normalized = TRUE),
                         eigen_centrality=0,
                         noSubgraphs=0, stringsAsFactors = FALSE)

    # Calculate the eigen centrality score
    eigen_centrality_score=igraph::eigen_centrality(g, directed = TRUE)[1]

    # Dismantle the graph to calculate its impact
    for (idx in 1:length(articulation_points)) {
      # Set the current articulation point
      currentAP <- articulation_points[idx]

      # Set a temp graph without the current articulation point
      tempGraph <- delete_vertices(g, currentAP)

      # Calculate the number and size of the disconnected components
      components <- igraph::components(tempGraph)

      # Select the eigen centrality score for each AP
      eigen_centrality_id = which(names(eigen_centrality_score$vector)==as_ids(currentAP))
      result[idx, 'eigen_centrality'] <- eigen_centrality_score$vector[eigen_centrality_id]

      # Evaluate each subgraph particularly
      for (idx2 in 1:components$no) {
        # Select specifically the components of each subgraph
        subgraph <- igraph::induced_subgraph(g, vids=components$membership==idx2)

        # Set the subgraph column name
        subgraphColumn <- paste0('subgraph', idx2)

        # Check if the subgraph column already exists in the dataframe
        if ( paste0(subgraphColumn, '_size') %in% colnames(result) == F ) {
          # Add the subgraph column
          result[paste0(subgraphColumn, '_size')] <- NA
          result[paste0(subgraphColumn, '_community')] <- NA
          result[paste0(subgraphColumn, '_mean_degree')] <- NA
        }

        # Fill the subgraph info
        result[idx, 'noSubgraphs'] <- components$no # components count
        result[idx, paste0(subgraphColumn, '_size')] <- components$csize[idx2] # subgraph size
        c3 <- igraph::cluster_walktrap(subgraph) # subgraph communities
        result[idx, paste0(subgraphColumn, '_community')] <- max(as.integer(igraph::membership(c3)))
        result[idx, paste0(subgraphColumn, '_mean_degree')] <- mean(igraph::degree(subgraph, V(subgraph)))

        # Remove the temp var
        rm(subgraph, subgraphColumn)
      }

      # Remove the temp var
      rm(currentAP, tempGraph, components)
    }
  } else {
    result <- NULL
  }

  return(result)
}

getArticulationPointNeighbors <- function(graph_, verbose_=FALSE) {

  # Status message
  if (verbose_) {
    print("Calculating the graph articulation points...")
  }

  # Select only the columns containing the nodes
  graph_ <- graph_[,c('node1', 'node2')]

  # Convert the graph into iGraph object
  g <- igraph::graph_from_data_frame(graph_, directed = TRUE)

  # Calculate the articulation points
  articulation_points <- getGraphBottleneck(g)

  # Status message
  if (verbose_) {
    print("Calculating the articulation points impact...")
  }

  if (!is.null(articulation_points) && length(articulation_points) > 0) {
    # Unify the graph nodes and set its community
    result <- data.frame(ap=as_ids(articulation_points),
                         eccentricity=igraph::eccentricity(g, articulation_points),
                         degree=igraph::degree(g, articulation_points),
                         closeness=igraph::closeness(g, articulation_points),
                         betweenness=igraph::betweenness(g, articulation_points, normalized = TRUE),
                         eigen_centrality=0,
                         networkDiameter=igraph::diameter(g, directed = TRUE),
                         neighborsL1=0, stringsAsFactors = FALSE)

    # Calculate the eigen centrality score
    eigen_centrality_score=igraph::eigen_centrality(g, directed = TRUE)[1]

    # Set the current neighbor equal to the node degree
    result$neighborsL1 <- result$degree

    # Set the current neighborhood level
    # Note: Level 0 = the self node
    # Level 1 = First level neighboors
    # Level 2 = Second level neighboors
    # Level N = Nth level neighboors
    neighborhoodLevel = 2

    # Dismantle the graph to calculate its impact
    for (idx in 1:length(articulation_points)) {
      # Set the current articulation point
      currentAP <- articulation_points[idx]

      # Select the eigen centrality score for each AP
      eigen_centrality_id = which(names(eigen_centrality_score$vector)==as_ids(currentAP))
      result[idx, 'eigen_centrality'] <- eigen_centrality_score$vector[eigen_centrality_id]

      # Evaluate each subgraph particularly
      for (level in neighborhoodLevel:result[idx,]$networkDiameter) {
        # Check if the current neighborhood level column already exists in the dataframe
        if ( paste0('neighborsL', level) %in% colnames(result) == F ) {
          # Add the subgraph column
          result[paste0('neighborsL', level)] <- NA
        }

        # Set the current neighborhood level
        # Note: We subtracted 1 since the current node shouldn't be accounted
        result[idx, paste0('neighborsL', level)] <- ego_size(g, order = level, nodes = currentAP, mode = "all") - 1
      }
    }
  } else {
    result <- NULL
  }

  return(result)
}

#*******************************************************************************************#

# ---- AUXILIARY FUNCTIONS ----

#' Function to remove unnecessary from igraph object
#'
#' @param iGraph_ Pathway iGraph object.
#' @param verbose_ Whether or not print the status message
#'
#' @return This function returns the igraph object cleared.
#'
#' @examples
#' \dontrun{
#' removeNoise(iGraph)
#' }
#'
#' @author
#' Igor Brandão

removeNoise <- function(iGraph_, verbose_=FALSE) {
  # Status message
  if (verbose_) {
    printMessage("REMOVING THE PATHWAY NOISE...")
  }

  if (is.null(iGraph_$node1) | length(iGraph_$node1) == 0) {
    # Remove unnecessary data from igraph object
    iGraph_ <- iGraph_[!grepl("^path:", iGraph_$name),]
    iGraph_ <- iGraph_[!grepl("^map:", iGraph_$name),]
    iGraph_ <- iGraph_[!grepl("^cpd:", iGraph_$name),]
    iGraph_ <- iGraph_[!grepl("^gl:", iGraph_$name),]
  } else {
    # Remove unnecessary data from igraph object
    iGraph_ <- iGraph_[!grepl("^path:", iGraph_$node1),]
    iGraph_ <- iGraph_[!grepl("^path:", iGraph_$node2),]
    iGraph_ <- iGraph_[!grepl("^map:", iGraph_$node1),]
    iGraph_ <- iGraph_[!grepl("^map:", iGraph_$node2),]
    iGraph_ <- iGraph_[!grepl("^cpd:", iGraph_$node1),]
    iGraph_ <- iGraph_[!grepl("^cpd:", iGraph_$node2),]
    iGraph_ <- iGraph_[!grepl("^gl:", iGraph_$node1),]
    iGraph_ <- iGraph_[!grepl("^gl:", iGraph_$node2),]
  }

  # Return the updated igraph object
  return(iGraph_)
}

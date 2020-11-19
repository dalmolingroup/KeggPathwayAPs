##########################
# Dependencies installer #
##########################

loadDependencies <- function(){
  # dependencies_installer.R #
  
  #' Install the necessary dependencies to run the main
  #' pipeline
  #'
  #' @author
  #' Igor Brandão / Clovis Reis
  #' 
  
  # Import the necessary libraries
  
  # Bioconductor installer
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  # KEGGgraph
  if (!requireNamespace("KEGGgraph", quietly = TRUE))
    BiocManager::install("KEGGgraph")
  library(KEGGgraph)

  # XML
  if (!requireNamespace("XML", quietly = TRUE))
    install.packages("XML", repos = "http://cran.us.r-project.org")
  
  # igraph
  if (!requireNamespace("igraph", quietly = TRUE))
    install.packages("igraph", repos = "http://cran.us.r-project.org")
  
  # RCurl
  if (!requireNamespace("RCurl", quietly = TRUE))
    install.packages("RCurl", repos = "http://cran.us.r-project.org")
  
  # rvest
  if (!requireNamespace("rvest", quietly = TRUE))
    install.packages("rvest", repos = "http://cran.us.r-project.org")
  
  # stringr
  if (!requireNamespace("stringr", quietly = TRUE))
    install.packages("stringr", repos = "http://cran.us.r-project.org")
  
  # pracma
  if (!requireNamespace("pracma", quietly = TRUE))
    install.packages("pracma", repos = "http://cran.us.r-project.org")
  
  # foreach
  if (!requireNamespace("foreach", quietly = TRUE))
    install.packages("foreach", repos = "http://cran.us.r-project.org")
  
  # Import functions files
  source(file.path(funcDir,"graphFunctions.R"))
  source(file.path(funcDir,"kgmlFunctions.R"))
  source(file.path(funcDir,"helperFunctions.R"))
  source(file.path(funcDir,"phaseFunctions.R"))
  
}
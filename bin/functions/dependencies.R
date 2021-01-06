#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

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

  # XML2
  if (!requireNamespace("xml2", quietly = TRUE))
    install.packages("xml2", repos = "http://cran.us.r-project.org")
  library(xml2)
  
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
  
  # SQLite
  if (!requireNamespace("DBI", quietly = TRUE))
    install.packages("DBI", repos = "http://cran.us.r-project.org")
  library(DBI)
  
  # tidyr
  if (!requireNamespace("tidyr", quietly = TRUE))
    install.packages("tidyr", repos = "http://cran.us.r-project.org")
  library(tidyr)
 
  # tidyverse
  if (!requireNamespace("tidyverse", quietly = TRUE))
    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  library(tidyverse) 
  
  # doParallel
  if (!requireNamespace("doParallel", quietly = TRUE))
    install.packages("doParallel", repos = "http://cran.us.r-project.org")
  library(doParallel)
  
  #BiocManager::install("Rgraphviz")
  #BiocManager::install("RBGL")
  
  # Import functions files
  source(file.path(funcDir,"graphFunctions.R"))
  source(file.path(funcDir,"kgmlFunctions.R"))
  source(file.path(funcDir,"helperFunctions.R"))
  source(file.path(funcDir,"phaseFunctions.R"))
  source(file.path(funcDir,"dbFunctions.R"))
  
}


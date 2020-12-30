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

# Phase 02 ----
# Analise XML files from KEGG and transform it in usable information
# clean redundant data 
# create unique node IDs
# create a dictionary from ec files
# save a new filtered XML file
# save base files for network metrics
generateDataFromKGML <- function(dirBase,
                        dataType = "ec",
                        skip = T) {
  #Parameters:
  #   dirBase - name of your work folder
  #   dataType - Use ec to porcess ec pathways and ko for ko pathways
  #   skip - don't execute this phase
  # the data will be stored inside the database
  
  
  dataType = "ec" #debug
  if(skip){
    cat("Skiping", dataType,  "pathways processing... \n\n")
    return(0)
  }
  
  logFile <<- file.path(dirBase,"log","phase2.log")

  cat("Starting", dataType,  "pathways processing... \n\n")
  cat(file = logFile, append = F,
      "\n\n ***************************************************\n",
      "Starting", dataType,  "pathways processing...\n",
      date(),"\n",
      "***************************************************\n\n")
  
  
  workDir<-file.path(dirBase,"data","kgml")
  filterDir<-file.path(dirBase,"data","filtredKgml",dataType)
  if(!dir.exists(filterDir)){
    dir.create(filterDir, recursive = T)
  }
  dbDir<-file.path(dirBase,"data","database")
  dbTemplate<-file.path(dbDir,"APs.sql")
  dbFile<-file.path(dbDir,"dictionary.db")
  # Define the reference pathway
  if(dataType == 'ec'){
    folders <- file.path(workDir,"ec")
    #check if db folder not exists and create it
    if(!dir.exists(dbDir)){
      dir.create(dbDir)
    }
    #conect and test dictionary
    dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
    #check if is an empty db. If it is, copy from template
      if(length(dbListTables(dbCon)) == 0){
      dbDisconnect(dbCon)
      if(file.exists(dbTemplate)){
        # file.copy(dbTemplate, dbFile, overwrite = T)
        createDB(dbTemplate, dbFile)
      }else{
        stop("Database template is missing. Please download it from github...")
      }
      dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
    }
  }else{
    #list all folders
    folders<-list.dirs(workDir,full.names = T)
    #remove ec and kgml folders
    folders<-folders[!folders %in% c(workDir,
                                     file.path(workDir,"ec"))]
  }
  folder<- folders[1] #debug
  for(folder in folders){
    # Get the list of files
    #folder = file.path("./output/kgml/", reference_pathway, "/")
    kgml_list <- list.files(path=folder, pattern='*.xml')
    # xmllist ----
    #*********************************************************************************#
    # IMPORTANTE: Aqui estou reduzindo a lista das pathways para não ficar tão pesado!
    #*********************************************************************************#
    #kgml_list = c('ec00010.xml', 'ec00520.xml') #debug
    #kgml_list = c('ec00010.xml') #debug
    
    kgml_index <- 1
    
    # Define the number of available pathways
    available_pathways <- length(kgml_list)
    
    # Check if the folder contains files
    if (is.null(kgml_list) | length(kgml_list) == 0) {
      # Status message
      stop("There aren't available pathways...")
      return(FALSE)
    }
    file = kgml_list[1] #debug
    processed <<- 0
    # Loop 01: Run through all available pathways kgml
    lapply(kgml_list, function(file) {
      processed <<- processed + 1
      cat("Processing",
          file,
          '[',processed,'/',
          available_pathways,']\n')
      cat(file = logFile, append = T,
          "Processing", file,'...\n')
      
      kgml_<-file.path(folder, file) #debug
      # Load the dataframe ----
      current_kgml <- KGML2Dataframe(kgml_, dataType, dbCon)
    })
      
    setSecondaryCompounds()
    keggErrorsFix()
    
  #     #parei aqui ----      
  #     # Get the pathway code
  #     pathway_code <- onlyNumber(file)
  #     #treat the nodes to create unique identifiers for all maps
  #     #save(current_kgml,file="/home/clovis/Projetos/GrupoDalmolin/Igor/essencialidade/bin/calculoAP/kgml.RData")
  #     
  #     
  #     # Status message
  #     printMessage(paste0("COUNTING ", pathway_code, " ENZYMES FREQUENCIES [", kgml_index, " OF ", available_pathways, "]"))
  #     
  #     #    tryCatch({
  #     # Convert the pathway data into a graph
  #     pathwayGraphOrig <- KGML2Graph(file.path(folder, file), replaceOrg=TRUE, orgToReplace=reference_pathway)
  #     
  #     tec<-c('ec:3.1.3.10','ec:5.4.2.2','ec:3.1.3.9','ec:2.7.1.1','ec:2.7.1.63',
  #          'ec:2.7.1.2','ec:2.7.1.147','ec:2.7.1.199','ec:5.1.3.3','ec:5.1.3.15',
  #          'ec:5.3.1.9')
  #     treaction<-c('ec:3.1.3.10','ec:5.4.2.2','ec:3.1.3.9','ec:2.7.1.1','ec:2.7.1.63',
  #          'ec:2.7.1.147','ec:2.7.1.199','ec:5.1.3.3','ec:5.3.1.9')
  #     tspros<-c('ec:3.1.3.10','ec:5.4.2.2','ec:3.1.3.9','ec:2.7.1.1',
  #               'ec:2.7.1.199','ec:5.1.3.3','ec:5.3.1.9')
  #     teste<-aux[aux$node1 %in% tspros & aux$node2 %in% tspros,]
  #     gggg<-igraph::graph_from_data_frame(teste, directed = TRUE)
  #     tkplot(gggg)
  #     
  #     t1<-pathwayGraphOrig[,c("node1","node2")]
  #     t2<-pathwayGraphOrig[,c("node2","node1")]
  #     colnames(t2)<-c("node1","node2")
  #     t1<-rbind(t1,t2)
  #     unique(t1)
  #     sum(duplicated(t1,incomparables = F))
  #     
  #     #criate valid edges
  #     nodes<-current_kgml$nodes[,c('eId', 'eName', 'eReaction')]
  #     edges<-current_kgml$edges[,c("entry1","entry2")]
  #     
  #     edges<-merge(edges,nodes[,c("eId","eName",'eReaction')], 
  #                  by.x="entry1", by.y = "eId")
  #     edges<-merge(edges,nodes[,c("eId","eName")], by.x="entry2", by.y = "eId")
  #     tmp<-unique(reactionsRef[reactionsRef$rName %in% edges$eReaction,
  #                              c("rName","rType")])
  #     edges<-merge(edges,tmp,
  #                  by.x="eReaction", by.y="rName")
  #     
  #     
  #     edges$org<-'ec'
  #     edges$pathway<-pathway_code
  #     edges$eReaction<-NULL
  #     edges$entry1<-NULL
  #     edges$entry2<-NULL
  #     
  #     colnames(edges)<-c("node1", "node2","rType", "org", "pathway")
  #     reversible<-edges[edges$rType == "reversible", 
  #                      c("node2", "node1","rType", "org", "pathway")]
  #     colnames(reversible)<-c("node1", "node2","rType", "org", "pathway")
  #     edges<-rbind(edges,reversible)
  #     edges$rType<-NULL
  #     
  #     path1<-c(grep(pattern = "path:",pathwayGraphOrig$node1  ),
  #              grep(pattern = "path:",pathwayGraphOrig$node2  ))
  #     path1<-unique(path1)
  #     path2<-c(1:nrow(pathwayGraphOrig))
  #     path2<-path2[-path1]
  #     
  #     tmp1<-pathwayGraphOrig[path1,]
  #     tmp2<-pathwayGraphOrig[path2,]
  #     tmp2<-tmp2[order(tmp2$node1,tmp2$node2),]
  #     
  #     tmp3<-edges[order(edges$node1,edges$node2),]
  #     
  #     tmp4<-tmp2[tmp2$node1 == tmp3$node1 & tmp2$node2 == tmp3$node2]
  #     
  #     temp<- pathwayGraphOrig[pathwayGraphOrig$node1 %in% edges$node1,]
  #     temp<- temp[temp$node2 %in% edges$node2,]
  #     
  #     resto<-merge(tmp2,tmp3, by=c("node1","node2"), all = T)
  #     duplicated(temp)
  #     #*************************##
  #     # Prepare the pathway data #
  #     #*************************##
  #     
  #     # Create the pathwayData dataFrame
  #     pathwayData <- current_kgml$nodes[,c('eId', 'eName', 'eReaction')]
  #     
  #     # Add the default columns
  #     pathwayData$reaction_type <- NA
  #     pathwayData$org <- reference_pathway
  #     pathwayData$pathway <- pathway_code
  #     pathwayData$is_bottleneck <- 0
  #     
  #     pathwayData$freq <- 0
  #     pathwayData$freq_mean <- 0
  #     pathwayData$total_species <- 0
  #     pathwayData$percentage <- 0
  #     pathwayData$percentage_mean <- 0
  #     
  #     pathwayData$betweenness <- NA
  #     pathwayData$connectivity <- NA
  #     pathwayData$triangles <- NA
  #     pathwayData$clusteringCoef <- NA
  #     pathwayData$closenessCoef <- NA
  #     pathwayData$community <- NA
  #     pathwayData$eigenvectorScore <- NA
  #     pathwayData$eccentricity <- NA
  #     pathwayData$radius <- NA
  #     pathwayData$diameter <- NA
  #     pathwayData$degree <- NA
  #     pathwayData$authorityScore <- NA
  #     pathwayData$hubScore <- NA
  #     
  #     pathwayData$bottleneck_classification <- NA
  #     
  #     # Remove unnecessary data from pathway data/graph
  #     if (removeNoise_) {
  #       pathwayData <- removeNoise(pathwayData)
  #       pathwayGraphOrig <- removeNoise(pathwayGraphOrig)
  #     }
  #     
  #     # Assign the reaction type to each node
  #     for (idx in 1:nrow(pathwayData)) {
  #       for (idx2 in 1:length(current_kgml$reactions$name)) {
  #         # Check the position of the current reaction in reactions list
  #         if (current_kgml$reactions$name[idx2] %in% pathwayData[idx,]$reaction) {
  #           pathwayData[idx,]$reaction_type <- current_kgml$reactions$type[idx2]
  #           break()
  #         }
  #       }
  #     }
  #     
  #     # Get the graph properties
  #     graphProperties<-getGraphProperties(edges)
  #     graphPropertiesOrig <- getGraphProperties(pathwayGraphOrig)
  #     
  #     # Perform the graph bottleneck calculation
  #     iGraph <- igraph::graph_from_data_frame(edges, directed = TRUE)
  #     graphBottleneck <- igraph::as_ids(getGraphBottleneck(iGraph, FALSE))
  #     
  #     iGraphOrig <- igraph::graph_from_data_frame(pathwayGraphOrig, directed = TRUE)
  #     graphBottleneckOrig <- igraph::as_ids(getGraphBottleneck(iGraphOrig, FALSE))
  #     
  #     # Assign the bottlenecks for enzyme code (ec)
  #     if (strcmp(reference_pathway, 'ec')) {
  #       pathwayData$is_bottleneck[which(pathwayData$name %in% graphBottleneck)] <- 1
  #     }
  #     
  #   })
  #   #}
  # }
    
  }
  dbDisconnect(dbCon)
}

# Phase 03 ----
# Analise database and create edges information based on ec maps
# insert edges information
# create edges on each path
# create virtual nodes
# save data inside sqlite database
createNodesFromEC <- function(dirBase,
                              dataType = "ec",
                              skip = T) {
  #Parameters:
  #   dirBase - name of your work folder
  #   dataType - Use ec to porcess ec pathways and ko for ko pathways
  #   skip - don't execute this phase
  # the data will be stored inside the database
  
  
  dataType = "ec" #debug
  if(skip){
    cat("Skiping", dataType,  "pathways processing... \n\n")
    return(0)
  }
  
  logFile <<- file.path(dirBase,"log","phase3.log")
  
  cat("Starting", dataType,  "edges processing... \n\n")
  cat(file = logFile, append = F,
      "\n\n ***************************************************\n",
      "Starting", dataType,  "edges processing...\n",
      date(),"\n",
      "***************************************************\n\n")
  
  
  dbDir<<-file.path(dirBase,"data","database")
  dbFile<<-file.path(dbDir,"dictionary.db")
  # Define the reference pathway
  if(dataType == 'ec'){
    #check if db folder not exists
    if(!dir.exists(dbDir)){
      stop("Database is missing...")
    }
    #conect and test dictionary
    dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
    #check if is an empty db. If it is, copy from template
    if(length(dbListTables(dbCon)) == 0){
      dbDisconnect(dbCon)
        stop("Database is missing. Please, run phase 2 first...")
    }
    createNodes()
  }
  dbDisconnect(dbCon)
}

createGraphMetrics <- function(pathways = 'all',
                               skip = T){
  
  if(skip){
    cat("Skiping metrics generation... \n\n")
    return(0)
  }
  logFile <<- file.path(dirBase,"log","phase4.log")
  
  cat("Starting graph metric generation... \n\n")
  cat(file = logFile, append = F,
      "\n\n ***************************************************\n",
      "Starting graph metric generation...\n",
      date(),"\n",
      "***************************************************\n\n")
  
  
  dbDir<<-file.path(dirBase,"data","database")
  dbFile<<-file.path(dbDir,"dictionary.db")
  #check if db folder not exists
  if(!dir.exists(dbDir)){
    stop("Database is missing...")
  }
  #conect and test dictionary
  dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
  #check if is an empty db. If it is, copy from template
  if(length(dbListTables(dbCon)) == 0){
    dbDisconnect(dbCon)
    stop("Database is missing. Please, run phase 2 first...")
  }
  
  cleanMetrics()
  if(pathways == 'all'){
    pathways <- getAllPathways()
  }
  count = 1
  total = length(pathways)
  for(pathway in pathways){
    cat("Generating metrics for ", pathway,"[", count,"of",total,"]\n")
    cat(file = logFile, append = F,
        "Generating metrics for ", pathway,"... \n")
    insertMetrics(pathway)
    count <- count +1
  }
  dbDisconnect(dbCon)
  
}

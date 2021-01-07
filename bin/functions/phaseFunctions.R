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

# Phase 02 and 05 ----
# Analise XML files from KEGG and transform it in usable information
# clean redundant data 
# create unique node IDs
# create a dictionary from ec files
# correlate orgs information with ec information
# save base files for network metrics
generateDataFromKGML <- function(dirBase,
                        dataType = "ec",
                        skip = T) {
  #Parameters:
  #   dirBase - name of your work folder
  #   dataType - Use ec to porcess ec pathways and ko for ko pathways
  #   skip - don't execute this phase
  # the data will be stored inside the database
  
  
  #dataType = "orgs" #debug
  if(skip){
    cat("Skiping", dataType,  "pathways processing... \n\n")
    return(0)
  }
  
  workDir<-file.path(dirBase,"data","kgml")
  filterDir<-file.path(dirBase,"data","filtredKgml",dataType)
  if(!dir.exists(filterDir)){
    dir.create(filterDir, recursive = T)
  }
  dbDir<-file.path(dirBase,"data","database")
  dbTemplate<-file.path(dbDir,"APs.sql")
  dbFile<-file.path(dbDir,"dictionary.db")
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
  
  
  # Define the reference pathway
  if(dataType == 'ec'){
    #ECs ----
    #******************************
    #  Processing ECs 
    #******************************
    logFile <<- file.path(dirBase,"log","phase2.log")
    
    cat("Starting", dataType,  "pathways processing... \n\n")
    cat(file = logFile, append = F,
        "\n\n ***************************************************\n",
        "Starting", dataType,  "pathways processing...\n",
        date(),"\n",
        "***************************************************\n\n")
    
    
    folder <- file.path(workDir,"ec")

    # Get the list of files
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
    ecOk <- lapply(kgml_list, function(file) {
      processed <<- processed + 1
      cat("Processing",
          file,
          '[',processed,'/',
          available_pathways,']\n')
      cat(file = logFile, append = T,
          "Processing", file,'...\n')
      
      kgml_<-file.path(folder, file) #debug
      # Load the dataframe ----
      currentError <- KGML2Dataframe(kgml_, dataType, dbCon)
      if(currentError != 0){
        OkNr <- sub(pattern = 'ec',
                       replacement = '',
                       x = sub(pattern = '.xml',
                               replacement = '',
                               x=file))
        return(OkNr)
      }else{
        return(NA)
      }
      
    })
    
    ecOk<-na.exclude(do.call(rbind,ecOk))
    save(file=file.path(dirBase,
                        'data',
                        'database',
                        'ecOk.RData'),
              list = 'ecOk')
    setSecondaryCompounds()
    keggErrorsFix()
    
    
  }else{
    #Organisms ----
    #******************************
    #  Processing Organisms 
    #******************************
    
    #clean nodebyorgs table
    #prepareNodeByOrgs()
    logFile <<- file.path(dirBase,"log","phase5.log")
    
    cat("Starting organism pathways processing... \n\n")
    cat(file = logFile, append = T,
        "\n\n ***************************************************\n",
        "Starting organism pathways processing...\n",
        date(),"\n",
        "***************************************************\n\n")
    ecOk <- file.path(dirBase,
                          'data',
                          'database',
                          'ecOk.RData')
    if(file.exists(ecOk)){
      load(file = ecOk)
    }else{
      ecOk <- NA
    }
    #list all folders
    folders<-list.dirs(workDir,full.names = T)
    #remove ec and kgml folders
    folders<-folders[!folders %in% c(workDir,
                                     file.path(workDir,"ec"))]

    folder<- folders[1] #debug
    #folder<-'/home/clovis/Dropbox/projetos/Igor/gitAPs/data/kgml/hsa' #debug
    for(folder in folders){
      orgName <- basename(folder)
      cat("\n\n ***************************************************\n",
          "Starting ", orgName," pathways processing... \n",
          "***************************************************\n\n")
      cat(file = logFile, append = T,
          "\n\n ***************************************************\n",
          "Starting ", orgName," pathways processing...\n",
          date(),"\n",
          "***************************************************\n\n")
      # Get the list of files
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
        currentFile <<- file
        processed <<- processed + 1
        cat("Processing",
            file,
            '[',processed,'/',
            available_pathways,']\n')
        cat(file = logFile, append = T,
            "Processing", file,'...\n')
        
        fileNr <- sub(pattern = orgName,
                       replacement = '',
                       x = sub(pattern = '.xml',
                               replacement = '',
                               x=file))
        if(!fileNr %in% ecOk){
          cat("\tSkipping",
              file,
              '( EC error )\n')
          cat(file = logFile, append = T,
              "\tSkipping",
                  file,
                  '( EC error )\n')
          return(0)
        }
        
        kgml_<-file.path(folder, file) #debug
        dataType <- orgName #debug
        # Load the dataframe ----
        current_kgml <- KGML2Dataframe(kgml_, orgName, dbCon)
      })
      
      # setSecondaryCompounds()
      # keggErrorsFix()
    }
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

# Phase 04 ----
# Create the node metrics based on ec maps
# insert nodes information for each path
# save data inside sqlite database
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

# Phase 06 ----
# Calculate statistics 
# plot different graphics
analiseData <- function(skip = T){
  
  if(skip){
    cat("Skiping data analisys... \n\n")
    return(0)
  }
  
  #get list of organisms that have 
  #a sufficient number of enzymes
  orgs<-enzymeDistrib(sdFactor = 1, 
                      quiet = F,
                      plot = T,
                      save = T)
  
  
}

#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#*************************************#
# Functions to handle database        #
#*************************************#

# dbFunctions.R #

# ---- IMPORT SECTION ----

#' This is the set of functions to create and access the node database
#'
#' @author
#' Clovis F Reis

# Import the necessary libraries
library("DBI")

getNextId <- function(table){
  # ids for each table
  ids<-c("node"="nId",
    "path"="pId",
    "reaction"="rId",
    "enzime"="eId",
    "compound"="cId")
  sql<-paste0('SELECT max(',ids[table],')+1 as nextId
                FROM ',table,';')
  nextId <- as.numeric(dbGetQuery(dbCon,sql)[1,1])
  
  #very first ID
  if(is.na(nextId)){
    nextId <- 1
  }
  return(nextId)
}

insertPathInfo<-function(pathwayinfo){
  table <- "path"
  field <- "pName"

  pName<- sub(pattern = "path:",replacement = '', x=pathwayinfo$name)
  pDesc<-pathwayinfo$title
  pImage<-pathwayinfo$image
  pLink <- pathwayinfo$link
  
  #pathway exists?
  nextId <- searchValue(table, field, pName)
  if(nextId !=0 ){ # Exists
    return(list(nextId, pName))
  }
  # Inexists
  #take the next ID from database
  nextId<-getNextId(table)
  
  #new pathway
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               nextId,',"',
               pName,'","',
               pDesc,'","',
               pImage,'","',
               pLink,'");')
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
           error = function(e) {
             erro<-grep(pattern = "UNIQUE constraint failed",
                        x= e$message ) 
             if(length(erro)>0){
               warning(paste("Path",pName, "already exixts."))
             }else{
               warning(paste("Could not process",pName, "information.",e$message))
             }
             return(0)
             })
  if(e == 0){
    return(list(NA,pName))
  }else{
    return(list(nextId,pName ))
  }
}


insertEnzReac<-function(ltEnzReac){
  #Insert enzyme and reaction information into db
  enzimes <- ltEnzReac[[1]]
  reactionsRef <- ltEnzReac[[2]]
  reactionsDef <- ltEnzReac[[3]]
  
  reactionsRef <- do.call(rbind,
                 apply(X = reactionsRef,
                       MARGIN = 1,
                       insertReaction))
  
  
  table <- "enzime"
  field <- "eName"
  eName <- enzime["eName"]
  pId <- enzime["pId"]
  x <- enzime["x"]
  y <- enzime["y"]
  eLabel<-''
  
  #enzime exists?
  nextId <- searchValue(table, field, eName)
  if(nextId !=0 ){ # Exists
    # enzime in pathway exists
    exist<-searchValue("enzOnPath","eId",nextId, pId)
    if( exist == 0){
      sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                    nextId,',',
                    pId,',',
                    x,',',
                    y,');')
      resQuery <- dbExecute(dbCon,sql) 
    }
    return(list(nextId, eName))
  }
  # Inexists
  #take the next ID from database
  nextId<-getNextId(table)
  
  #new enzimeID
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               nextId,',"',
               eName,'","',
               eLabel,'");')
  resQuery <- dbExecute(dbCon,sql)
  sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                nextId,',',
                pId,',',
                x,',',
                y,');')
  
  #new pathway
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
              error = function(e) {
                erro<-grep(pattern = "UNIQUE constraint failed",
                           x= e$message ) 
                if(length(erro)>0){
                  warning(paste("Enzime",eName, "already exixts."))
                }else{
                  warning(paste("Could not process",eName, "information."))
                }
                return(0)
              })
  if(e == 0){
    return(list(NA,eName))
  }else{
    return(list(nextId,eName ))
  }
}

#add new Id for enzimes
enzimes<-merge(enzimes,do.call(rbind, eId),by=2)
enzimes$newId<-paste0('e',enzimes$V1)
enzimes$V1<-NULL


insertEnzime<-function(enzime){
  table <- "enzime"
  field <- "eName"
  eName <- enzime["eName"]
  pId <- enzime["pId"]
  x <- enzime["x"]
  y <- enzime["y"]
  eLabel<-''
  oldId<-enzime["eId"]

  #enzime exists?
  nextId <- searchValue(table, field, eName)
  if(nextId !=0 ){ # Exists
    # enzime in pathway exists
    exist<-searchValue("enzOnPath","eId",nextId, pId)
    if( exist == 0){
      sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                    nextId,',',
                    pId,',',
                    x,',',
                    y,');')
      resQuery <- dbExecute(dbCon,sql) 
    }
    return(list(nextId, eName))
  }
  # Inexists
  #take the next ID from database
  nextId<-getNextId(table)
  
    #new enzimeID
    sql<- paste0('INSERT INTO ', 
                 table,
                 ' VALUES (',
                 nextId,',"',
                 eName,'","',
                 eLabel,'");')
    resQuery <- dbExecute(dbCon,sql)
      sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                    nextId,',',
                    pId,',',
                    x,',',
                    y,');')

  #new pathway
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
              error = function(e) {
                erro<-grep(pattern = "UNIQUE constraint failed",
                           x= e$message ) 
                if(length(erro)>0){
                  warning(paste("Enzime",eName, "already exixts."))
                }else{
                  warning(paste("Could not process",eName, "information."))
                }
                return(0)
              })
  if(e == 0){
    return(list(NA,eName))
  }else{
    return(list(nextId,eName ))
  }
}

reaction<-as.vector(reactionsRef[1,]) #debug
insertReaction<-function(reaction){
  table <- "reaction"
  field <- "rName"
  rName <- reaction["rName"]
  rType <- reaction["rType"]
  rType <- ifelse(rType == 'reversible', 1,0)
  oldId<-reaction["rId"]
  
  #reaction exists?
  nextId <- searchValue(table, field, rName)
  if(nextId !=0 ){ # Exists
    # reaction exists on pathway
    exist<-searchValue("enzOnPath","eId",nextId, pId)
    if( exist == 0){
      sql <- paste0('INSERT INTO reacOnPath
                    VALUES (',
                    nextId,',',
                    pId,');')
      resQuery <- dbExecute(dbCon,sql) 
    }
    return(data.frame(newId=paste0('r',nextId)
                      ,oldId= oldId,
                      rName = rName, 
                      rType = rType,
                      stringsAsFactors = F))
  }
  # Inexists
  #take the next ID from database
  nextId<-getNextId(table)
  
  #new enzimeID
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               nextId,',"',
               rName,'","',
               rType,'");')
  resQuery <- dbExecute(dbCon,sql)
  sql <- paste0('INSERT INTO reacOnPath
                    VALUES (',
                nextId,',',
                pId,');')
  
  #new pathway
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
              error = function(e) {
                erro<-grep(pattern = "UNIQUE constraint failed",
                           x= e$message ) 
                if(length(erro)>0){
                  warning(paste("Enzime",eName, "already exixts."))
                }else{
                  warning(paste("Could not process",eName, "information."))
                }
                return(0)
              })
  if(e == 0){
    return(data.frame(newId=NA, 
                      oldId= oldId,
                      rName = rName, 
                      rType = rType,
                      stringsAsFactors = F))
  }else{
    return(data.frame(newId=paste0('r',nextId)
                      ,oldId= oldId,
                      rName = rName, 
                      rType = rType,
                      stringsAsFactors = F))
  }

  
  
    
  
  #reversible must be 0 or 1
  if(!rReversible %in% c(0,1)){
    warning(paste("Reaction must be 0 for irreversible or 1 to reversible.
                  Not processing", rName))
    return(0)
  }
  
  table <- "reaction"
  nextId<-getNextId(table)
  
  #new pathway
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               nextId,',"',
               rName,'","',
               rReversible,'");')
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
              error = function(e) {
                erro<-grep(pattern = "UNIQUE constraint failed",
                           x= e$message ) 
                if(length(erro)>0){
                  warning(paste("Reaction",rName, "already exixts."))
                }else{
                  warning(paste("Could not process",rName, "information."))
                }
                return()
              })
  return(nextId)
}

compound<-as.vector(compounds[1,]) #debug
insertCompound<-function(compound){
  table <- "compound"
  field <- "cName"
  
  cName <- compound["eName"]
  cDesc <- compound["cDesc"]
  pId <- compound["pId"]
  x <- compound["x"]
  y <- compound["y"]
  oldId<-compound["eId"]
  
  #compound exists?
  nextId <- searchValue(table, field, cName)
  if(nextId !=0 ){ # Exists
    # enzime in pathway exists
    exist<-searchValue("compOnPath","cId",nextId, pId)
    if( exist == 0){
      sql <- paste0('INSERT INTO compOnPath
                    VALUES (',
                    nextId,',',
                    pId,',',
                    x,',',
                    y,');')
      resQuery <- dbExecute(dbCon,sql) 
    }
    return(data.frame(newId=paste0('c',nextId),
                      oldId= oldId,
                      cName = cName, 
                      cDesc =cDesc,
                      x = x,
                      y = y,
                      stringsAsFactors = F))
  }
  # Inexists
  #take the next ID from database
  nextId<-getNextId(table)
  
  #new compoundID
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               nextId,',"',
               cName,'","',
               cDesc,'");')
  resQuery <- dbExecute(dbCon,sql)
  sql <- paste0('INSERT INTO compOnPath
                    VALUES (',
                nextId,',',
                pId,',',
                x,',',
                y,');')
  
 
  e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
              error = function(e) {
                erro<-grep(pattern = "UNIQUE constraint failed",
                           x= e$message ) 
                if(length(erro)>0){
                  warning(paste("Compound",cName, "already exixts."))
                }else{
                  warning(paste("Could not process",cName, "information."))
                }
                return(0)
              })
  if(e == 0){
    return(data.frame(NA,
                      oldId= oldId,
                      cName = cName, 
                      cDesc =cDesc,
                      x = x,
                      y = y,
                      stringsAsFactors = F))
  }else{
    return(data.frame(newId=paste0('c',nextId),
                      oldId= oldId,
                      cName = cName, 
                      cDesc =cDesc,
                      x = x,
                      y = y,
                      stringsAsFactors = F))
               
  }
}

ecNodes2Db <- function(nodes, map){
  node2ec <- list()
  i=2 #debug
  # dbDisconnect(dbCon) #debug
  # dbCon <- dbConnect(RSQLite::SQLite(), dbFile)#debug
  
  for(i in 1:nrow(nodes)){
    #create unique node IDs for all maps
    sql<-paste0('SELECT * 
                FROM nodes
                WHERE name = "',nodes$eName[i],  '" and ',
                'reaction = "',nodes$eReaction[i],'"')
    resQuery <- dbGetQuery(dbCon,sql)
    #take the next ID from database
    nextId <- getNextId(nodes)
    #first node of this type
    if(nrow(resQuery)==0){
      nextId<- nextId
      #new nodeID
      sql<- paste0('INSERT INTO nodes
                    VALUES (',
                   nextId,',"',
                   nodes$eName[i],'","',
                   nodes$eReaction[i],'",',
                   nodes$x[i],',',
                   nodes$y[i],');')
      resQuery <- dbExecute(dbCon,sql)
      # Duplicated node from other maps
    }else{
      #insert just in nodeMaps
      nextId<-resQuery$nodeID
    }
    
    sql<- paste0('INSERT INTO nodeMaps
                    VALUES (',
                 nextId,',"',
                 map,'");')
    resQuery <- dbExecute(dbCon,sql)
  }
}

searchValue <- function(table, field, value, pId = NA){
  if(is.na(pId)){
    sql<-paste0('SELECT * 
                FROM ',table,
                ' WHERE ',field,' = "',value,  '"')
    resQuery <- dbGetQuery(dbCon,sql)
    if(nrow(resQuery)==0){
      return(0)
    }else{
      return(resQuery[[1]][[1]])
    }
  }else{
    sql<-paste0('SELECT * 
                FROM ',table,
                ' WHERE ',field,' = ',value,  ' and ',
                        ' pId = ', pId,';')
    resQuery <- dbGetQuery(dbCon,sql)
    if(nrow(resQuery)==0){
      return(0)
    }else{
      return(resQuery[[1]][[1]])
    }
  }
}

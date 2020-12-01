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
  fields <- "pName"

  pName<- sub(pattern = "path:",replacement = '', x=pathwayinfo$name)
  pDesc<-pathwayinfo$title
  pImage<-pathwayinfo$image
  pLink <- pathwayinfo$link
  
  values<-paste0('"',pName,'"')
  
  #pathway exists?
  nextId <- searchValue(table, fields, values)
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
  compounds<- ltEnzReac[[4]]
  pId <- ltEnzReac[[5]]
  #reactionsRef$pId <- pId
  
  
  reactionsRef <- do.call(rbind,
                 apply(X = reactionsRef,
                       MARGIN = 1,
                       insertReaction))

  reactionsDef<-merge(reactionsDef,
                       reactionsRef[,1:2], 
                       by.x= "rId",
                       by.y = "oldId")
  
  colnames(reactionsDef)<- c("rId","cpdType","cpdId",
                              "cpdName","pId","rNewId")
  reactionsDef<-merge(reactionsDef,
                       compounds[,1:2], 
                       by.x= "cpdId",
                       by.y = "oldId")
  colnames(reactionsDef)<- c("cpdId","rId","cpdType",
                              "cpdName","pId","rNewId",
                              "cNewId")
  reactionsDef<-do.call(rbind,
                         apply(X = reactionsDef,
                               MARGIN = 1,
                               insertSubsProd))
  
  enzimes <- merge(enzimes, 
                    reactionsRef[!duplicated(reactionsRef$newId),
                                  c("newId","rName")],
                    by.x="eReaction",
                    by.y = "rName")
  colnames(enzimes)<-c("eReaction","eId","eName","eType",
              "x","y","pId","rId")
  enzimes$eReaction<-NULL
  enzimes$rId <- substring(enzimes$rId,2)
  
  enzimes <- do.call(rbind,
                      apply(X = enzimes,
                            MARGIN = 1,
                            insertEnzime))
  
  return(list(enzimes,reactionsRef,reactionsDef))
  
}

# #add new Id for enzimes
# enzimes<-merge(enzimes,do.call(rbind, eId),by=2)
# enzimes$newId<-paste0('e',enzimes$V1)
# enzimes$V1<-NULL

#enzime<-enzimes[3,] #debug
insertEnzime<-function(enzime){
  table <- "enzime"
  fields <- "eName"
  
  oldId<-enzime["eId"]
  eName <- enzime["eName"]
  rId <- enzime["rId"]
  x <- enzime["x"]
  y <- enzime["y"]
  pId <- enzime["pId"]
  eLabel<-''

  values<-paste0('"',eName,'"')
  
  #enzime exists?
  nextId <- searchValue(table, fields, values)
  if(nextId !=0 ){ # Exists
    # enzime in pathway exists
    fields<-c("eId","pId")
    values<-c(paste0('"',nextId,'"'),
              paste0('"',pId,'"'))
    
    exist<-searchValue("enzOnPath",fields, values)
    if( exist == 0){
      sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                    nextId,',',
                    pId,',',
                    x,',',
                    y,');')
      resQuery <- dbExecute(dbCon,sql) 
      
      fields<-c("eId","rId")
      values<-c(paste0('"',nextId,'"'),
                paste0('"',rId,'"'))
      
      exist<-searchValue("enzReac",fields, values)
      if( exist == 0){
        sql<- paste0('INSERT INTO enzReac
                      VALUES (',
                     nextId,',',
                     rId,');')
        resQuery <- dbExecute(dbCon,sql) 
      }
    }
    return(data.frame(newId=paste0('e',nextId)
                      ,oldId= oldId,
                      eName = eName, 
                      x = x,
                      y = y,
                      pId = pId,
                      eLabel = eLabel, 
                      rId = paste0('r',rId),
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
                 eName,'","',
                 eLabel,'");')
    resQuery <- dbExecute(dbCon,sql)
    sql <- paste0('INSERT INTO enzOnPath
                    VALUES (',
                  nextId,',',
                  pId,',',
                  x,',',
                  y,');')
    resQuery <- dbExecute(dbCon,sql)

    sql<- paste0('INSERT INTO enzReac
                      VALUES (',
                 nextId,',',
                 rId,');')
    resQuery <- dbExecute(dbCon,sql)
    
    return(data.frame(newId=paste0('e',nextId)
                      ,oldId= oldId,
                      eName = eName, 
                      x = x,
                      y = y,
                      pId = pId,
                      eLabel = eLabel, 
                      rId = paste0('r',rId),
                      stringsAsFactors = F))
    
}

#reaction<-as.vector(reactionsRef[59,]) #debug
insertReaction<-function(reaction){
  table <- "reaction"
  fields <- c("rName","rReversible")
  rName <- reaction["rName"]
  rType <- reaction["rType"]
  rType <- ifelse(rType == 'reversible', 1,0)
  oldId<-reaction["rId"]
  pId <- reaction["pId"]
  
  values<-c(paste0('"',rName,'"'),
            paste0('"',rType,'"'))
  
  #reaction exists?
  nextId <- searchValue(table, fields, values)
  if(nextId !=0 ){ # Exists
    # reaction exists on pathway
    
    fields<-c("rId","pId")
    values<-c(paste0('"',nextId,'"'),
              paste0('"',pId,'"'))
    
    
    exist<-searchValue("reacOnPath",fields, values)
    if( exist == 0){
      sql <- paste0('INSERT INTO reacOnPath
                    VALUES (',
                    nextId,',',
                    pId,');')
      resQuery <- dbExecute(dbCon,sql) 
    # }else{ #debug
    #   cat("existe",rName," ",pId, " \n") #debug
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

  
  
    
  
  # #reversible must be 0 or 1
  # if(!rReversible %in% c(0,1)){
  #   warning(paste("Reaction must be 0 for irreversible or 1 to reversible.
  #                 Not processing", rName))
  #   return(0)
  # }
  # 
  # table <- "reaction"
  # nextId<-getNextId(table)
  # 
  # #new pathway
  # sql<- paste0('INSERT INTO ', 
  #              table,
  #              ' VALUES (',
  #              nextId,',"',
  #              rName,'","',
  #              rReversible,'");')
  # e<-tryCatch(resQuery <- dbExecute(dbCon,sql),
  #             error = function(e) {
  #               erro<-grep(pattern = "UNIQUE constraint failed",
  #                          x= e$message ) 
  #               if(length(erro)>0){
  #                 warning(paste("Reaction",rName, "already exixts."))
  #               }else{
  #                 warning(paste("Could not process",rName, "information."))
  #               }
  #               return()
  #             })
  # return(nextId)
}

#compound<-as.vector(compounds[1,]) #debug
insertCompound<-function(compound){
  table <- "compound"
  fields <- "cName"
  
  cName <- compound["eName"]
  cDesc <- compound["cDesc"]
  pId <- compound["pId"]
  x <- compound["x"]
  y <- compound["y"]
  oldId<-compound["eId"]
  
  values<-paste0('"',cName,'"')
  
  #compound exists?
  nextId <- searchValue(table, fields, values)
  if(nextId !=0 ){ # Exists
    # enzime in pathway exists
    fields<-c("cId","pId")
    values<-c(paste0('"',nextId,'"'),
              paste0('"',pId,'"'))
    
    exist<-searchValue("compOnPath",fields, values)
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


#relation<-as.vector(relationRef2[1,]) #debug
insertRelation<-function(relation){
  table <- "interaction"
  fields <- c("eId1","eId2","cId")
  
  eNewId1 <- relation["eNewId1"]
  eNewId2 <- relation["eNewId2"]
  cNewId <- relation["cNewId"]
  eOldId1 <- relation["eOldId1"]
  eOldId2 <- relation["eOldId2"]
  cOldId <- relation["cOldId"]
  
  values<-c(paste0('"',eNewId1,'"'),
            paste0('"',eNewId2,'"'),
            paste0('"',cNewId,'"'))
  
  #compound exists?
  nextId <- searchValue(table, fields, values)
  if(nextId !=0 ){ # Exists
    return(data.frame(eNewId1 = eNewId1,
                      eNewId2= eNewId2,
                      cNewId = cNewId, 
                      eOldId1 = eOldId1,
                      eOldId2 = eOldId2,
                      cOldId = cOldId,
                      stringsAsFactors = F))
  }
  # Inexists
  #new relation
  sql<- paste0('INSERT INTO ', 
               table,
               ' VALUES (',
               substring(eNewId1,2),',',
               substring(eNewId2,2),',',
               substring(cNewId,2),');')
  resQuery <- dbExecute(dbCon,sql)
 
  eNewId1 <- relation["eNewId1"]
  eNewId2 <- relation["eNewId2"]
  cNewId <- relation["cNewId"]
  eOldId1 <- relation["eOldId1"]
  eOldId2 <- relation["eOldId2"]
  cOldId <- relation["cOldId"] 
  
  return(data.frame(eNewId1 = eNewId1,
                    eNewId2= eNewId2,
                    cNewId = cNewId, 
                    eOldId1 = eOldId1,
                    eOldId2 = eOldId2,
                    cOldId = cOldId,
                    stringsAsFactors = F))
}

#rDef<-as.vector(reactionsDef2[1,]) #debug
insertSubsProd<-function(rDef){
  table <- "subsProd"
  fields <- c("rId","cId","spType")
  
  oldcId <- rDef["cpdId"]
  oldrId <- rDef["rId"]
  cType <- substr(rDef["cpdType"],1,1) 
  cName <- rDef["cpdName"]
  rNewId <- rDef["rNewId"]
  cNewId <- rDef["cNewId"]
  
  values<-c(paste0('"',substring(rNewId,2),'"'),
            paste0('"',substring(cNewId,2),'"'),
            paste0('"',cType,'"'))
  
  #compound exists?
  nextId <- searchValue(table, fields, values)
  if(nextId == 0 ){
    # Inexists

    #new compoundID
    sql<- paste0('INSERT INTO ', 
                 table,
                 ' VALUES ("',
                 substring(rNewId,2),'","',
                 substring(cNewId,2),'","',
                 cType,'");')
  resQuery <- dbExecute(dbCon,sql)
  
  return(data.frame(oldcId = oldcId,
                    oldrId = oldrId,
                    cType = cType, 
                    cName = cName,
                    rNewId = rNewId,
                    cNewId = cNewId,
                    stringsAsFactors = F))
  }else{
    return(data.frame(oldcId = oldcId,
                      oldrId = oldrId,
                      cType = cType, 
                      cName = cName,
                      rNewId = rNewId,
                      cNewId = cNewId,
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


searchValue <- function(table, fields, values){
  if(length(fields) != length(values)){
    stop("Fields and values must have same length.")
  }
  sql<-paste0('SELECT * 
                FROM ',table,
              ' WHERE ')
  for(idx in 1:length(fields)){
    sql<- paste0(sql,fields[idx],' = ',values[idx])
    if(idx != length(fields)){
      sql<- paste0(sql,' and ')
    }
  }
  sql
    resQuery <- dbGetQuery(dbCon,sql)
    if(nrow(resQuery)==0){
      return(0)
    }else{
      return(resQuery[[1]][[1]])
    }
}

# searchValue <- function(table, field, value, pId = NA){
#   if(is.na(pId)){
#     sql<-paste0('SELECT * 
#                 FROM ',table,
#                 ' WHERE ',field,' = "',value,  '"')
#     resQuery <- dbGetQuery(dbCon,sql)
#     if(nrow(resQuery)==0){
#       return(0)
#     }else{
#       return(resQuery[[1]][[1]])
#     }
#   }else{
#     sql<-paste0('SELECT * 
#                 FROM ',table,
#                 ' WHERE ',field,' = ',value,  ' and ',
#                         ' pId = ', pId,';')
#     resQuery <- dbGetQuery(dbCon,sql)
#     if(nrow(resQuery)==0){
#       return(0)
#     }else{
#       return(resQuery[[1]][[1]])
#     }
#   }
# }

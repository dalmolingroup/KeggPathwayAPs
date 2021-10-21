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


createDB <- function(dbTemplate, dbFile, skip = T){
  if(skip){
    cat("Skiping database creation... \n\n")
    return(0)
  }
  cat("Creating the database. Please wait... \n\n")
  command<-paste("cat ",dbTemplate,
                 " | sqlite3 ", dbFile)
  system(command)
  
}

unpackDB <- function(dbDir = dbDir,
         skip = T){
  if(skip){
    cat("Skiping database unpacking... \n\n")
    return(0)
  }
  dbFile <- file.path(dbDir,'dictionary.db')
  cat("Removing the old database. Please wait... \n")
  if(file.exists(dbFile)){
    file.remove(dbFile)   
  }else{
    cat("Old database not found... \n")
  }
  tarFile <- file.path(dbDir,'dictionary.db.tar.bz2')
  if(!file.exists(tarFile)){
    stop('File ', tarFile, ' not found...')    
  }
  cat("Unpacking the database. Please wait... \n\n")
  command<-paste0('tar -C ', dbDir,' -xjf ',tarFile)
  system(command)
  
  
}

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

getNextFakeEdge <- function(){
  sql <- 'select nextId
          from fakeEdge'
  nextId<-as.numeric(dbGetQuery(dbCon,sql)[1,1])

  sql <- paste0('update fakeEdge set nextId = ',nextId+1)
  resQuery <- dbExecute(dbCon,sql)
  
  return(nextId)
  # sql <- 'select max(nId) as nextId
  #         from edges
  #         where type = "F"'
  # nextId<-as.numeric(dbGetQuery(dbCon,sql)[1,1])
  # if(is.na(nextId)){
  #   nextId<-100000
  # }
  # return(nextId)
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
  #reactionsRef <- na.exclude(reactionsRef)

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

#enzime<-enzimes[28,] #debug
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
    # enzime exists in pathway?
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
    }
    
    # enzime and reaction exists in pathway?
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

#reaction<-as.vector(reactionsRef[29,]) #debug
insertReaction<-function(reaction){
  blFile <- file.path(dbDir,"blacklist")
  blackList<-read.csv(file = blFile,
                      header = T, 
                      stringsAsFactors =F)
    
    # c("rn:R02189","rn:R00150","rn:R00200","rn:R00212",
    #             "rn:R00214","rn:R00216","rn:R00228",
    #             "rn:R00229","rn:R00237","rn:R00238",
    #             "rn:R00243","rn:R00258","rn:R00262",
    #             "rn:R00264","rn:R00286","rn:R00289",
    #             "rn:R00341","rn:R00342","rn:R00343",
    #             "rn:R00352","rn:R00355","rn:R00372",
    #             "rn:R00396","rn:R00405","rn:R00431",
    #             "rn:R00462","rn:R00472","rn:R00473",
    #             "rn:R00529","rn:R00565","rn:R00586",
    #             "rn:R00588","rn:R00653","rn:R00672",
    #             "rn:R00707","rn:R00708","rn:R00710",
    #             "rn:R00715","rn:R00726","rn:R00734","rn:R00737","rn:R00742","rn:R00774","rn:R00782","rn:R00833","rn:R00858","rn:R00859","rn:R00883","rn:R00885","rn:R00889","rn:R00905","rn:R00921","rn:R00945","rn:R00946","rn:R00948","rn:R00954","rn:R00956","rn:R00994","rn:R01015","rn:R01049","rn:R01056","rn:R01061","rn:R01063","rn:R01087","rn:R01150","rn:R01151","rn:R01155","rn:R01177","rn:R01183","rn:R01196","rn:R01197","rn:R01214","rn:R01220","rn:R01280","rn:R01325","rn:R01366","rn:R01370","rn:R01371","rn:R01373","rn:R01381","rn:R01388","rn:R01392","rn:R01431","rn:R01434","rn:R01476","rn:R01512","rn:R01518","rn:R01529","rn:R01611","rn:R01641","rn:R01655","rn:R01669","rn:R01717","rn:R01728","rn:R01777","rn:R01786","rn:R01788","rn:R01836","rn:R01838","rn:R01900","rn:R01975","rn:R01976","rn:R01977","rn:R02000","rn:R02101","rn:R02164","rn:R02199","rn:R02219","rn:R02269","rn:R02340","rn:R02484","rn:R02536","rn:R02661","rn:R02695","rn:R02722","rn:R02729","rn:R02765","rn:R02963","rn:R03026","rn:R03027","rn:R03045","rn:R03046","rn:R03158","rn:R03172","rn:R03244","rn:R03264","rn:R03337","rn:R03339","rn:R03397","rn:R03399","rn:R03778","rn:R03858","rn:R03896","rn:R03898","rn:R03984","rn:R03991","rn:R04095","rn:R04125","rn:R04325","rn:R04560","rn:R04742","rn:R04747","rn:R05076","rn:R05399","rn:R05636","rn:R05692","rn:R05850","rn:R06613","rn:R07055","rn:R07618","rn:R07672","rn:R07675","rn:R07676","rn:R09097","rn:R10412","rn:R10782")
  
  table <- "reaction"
  fields <- c("rName","rReversible")
  rName <- reaction["rName"]
  rType <- reaction["rType"]
  if(rName %in% blackList$Reaction){
    rType <- blackList$Reversible[blackList$Reaction == rName]
  }else{
    rType <- ifelse(rType == 'reversible', 1,0)
  }
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
  
  #new reaction ID
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
                 cType,'",0);')
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


createNodes <- function(){
  #detect duplicated substrate product tuples
  # sql <- "select rId, spType || cId as cId
  # from subsProd
  # where secondary == 0
  # order by rId, spType DESC, cId"
  
  
  #reactions with problems
  #0 substrate or 0 product
  sql<-"SELECT rId 
        FROM (
          select tr.rId, tr.rName, 
          sum(CASE WHEN spType = 's' THEN 1 ELSE 0 END) as s,
          sum(CASE WHEN spType = 'p' THEN 1 ELSE 0 END) as p
          from subsProd as ts inner JOIN
          reaction as tr on tr.rid = ts.rid 
          where secondary = 0 
          GROUP by tr.rId
        )
        where s = 0 or p = 0"
  badReactions <- dbGetQuery(dbCon,sql)
  
  badReactions<-paste0(badReactions)
  badReactions<- substring(badReactions,2)
  
  
  
  # sql <- paste0("select *
  #   from subsProdNames
  #   where rId not in ",badReactions)
  #select all reactions except the bad ones

  sql <- paste0('select r.rId,
                        case when r.rReversible = 1 then "R" ELSE "I" END ||
                        " " || sp.spType || sp.cId as cId
                from subsProd as sp INNER JOIN
                    reaction as r on r.rId = sp.rId 
                where sp.secondary == 0 and 
                r.rId not in ',
                badReactions,
                " order by r.rId, sp.spType DESC, sp.cId")
  reactions <- dbGetQuery(dbCon,sql)
  
  
  tmp<-reactions %>% 
    group_by(rId) %>% 
    mutate(dup_count = sequence(dplyr::n()), 
           key = paste("cpd", dup_count, sep = "")) %>% 
    tidyr::spread(., 
                  key = key,
                  value = cId) %>% 
    tidyr::fill(dplyr::starts_with("cpd"), .direction = "up") %>% 
    dplyr::distinct(., rId, .keep_all = TRUE)
  
  
  reactions2<- do.call(rbind,
                      apply(X = tmp, MARGIN = 1,
                            FUN = function(x){
                              txt<-''
                              for(idx in 3:length(x)){
                                if(is.na(x[idx])){
                                  next
                                }
                                txt <- paste(txt, x[idx])
                              }
                              return(data.frame(rId=x[1],
                                                cpd=txt,
                                                stringsAsFactors = F))
                            }))
  reactList<-reactions2 %>% 
    group_by(cpd) %>% 
    mutate(dup_count = sequence(dplyr::n()), 
           key = paste("rId", dup_count, sep = "")) %>% 
    tidyr::spread(., 
                  key = key,
                  value = rId) %>% 
    tidyr::fill(dplyr::starts_with("rId"), .direction = "up") %>% 
    dplyr::distinct(., cpd, .keep_all = TRUE)
  
  #clean table and insert reaction correlations
  prepareReacAssos()
  lixo<-apply(X = reactList, MARGIN = 1,
        insertReacList)
  
  #create graph edges
  counter <<- 1
  total <<- nrow(reactList)
  lixo<-apply(X = reactList, MARGIN = 1,
        insertEdges)
  
  rm(counter, total)
  
  createNodeTable()
  
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
    if (class(values[idx]) == "list" && length(unlist(values[idx])) > 1) {
      sql<- paste0(sql,fields[idx],' IN (', sapply(values[idx], paste, collapse=", "), ')')
    } else {
      sql<- paste0(sql,fields[idx],' = ', values[idx])
    }
    
    if(idx != length(fields)){
      sql<- paste0(sql,' and ')
    }
  }
    resQuery <- dbGetQuery(dbCon,sql)
    #cat(sql,' ',nrow(resQuery),'\n')
    if(nrow(resQuery) > 1){
      return(resQuery)
    }else if(nrow(resQuery)==0){
      return(0)
    }else{
      return(resQuery[[1]][[1]])
    }
}

prepareReacAssos <- function(){
  #prepare the table reactionAssociation to receive data
  sql <- "DELETE FROM reactionAssociation"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM nodeAlias"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM nodes"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM edges"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM fakeNode"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "update fakeEdge set nextId = 100000"
  resQuery <- dbExecute(dbCon,sql)
  
  
  sql <- "INSERT INTO reactionAssociation
          SELECT DISTINCT rId, rId
          FROM reaction;"
  resQuery <- dbExecute(dbCon,sql)
}

prepareNodeByOrgs <- function(){
  #prepare the table reactionAssociation to receive data
  # sql <- "DELETE FROM nodebyorgs"
  # resQuery <- dbExecute(dbCon,sql)
}

#reactList2<-reactList[reactList$rId1=="  97",]
#reactList2<-reactList[1,] #debug
insertReacList <- function(reactList2){
  #make the correlation between reactions with same 
  # subtrate and product
  reactList2<-reactList2[-2]#$dup_count<-NULL
  reactList2<-as.vector((reactList2[!is.na(reactList2)]))
  #cat(reactList2,'\n')
  idx<-2
  #if(length(reactList2) > 2){
    for(idx in 2:length(reactList2)){
      sql <- paste0("select * 
                    from reactionAssociation
                    where rId = ", reactList2[idx]," and 
                    mainRId != 0;")
      resQuery <- dbGetQuery(dbCon,sql)
      if(resQuery[1,1] != resQuery[1,2]){
        cat("Reaction", reactList2[idx], 'already processed. Overwriting\n!')
      }
      sql<-paste0('UPDATE reactionAssociation
          SET mainRId = ',reactList2[2],'
          WHERE rId = ',reactList2[idx],';')
      resQuery <- dbExecute(dbCon,sql)
    }
  #}
}

insertEnzymeAssos<-function(){
  #create names and associations between enzymes
  
}

# reactList2<-reactList[78,] #debug
# insertEdges(reactList2)
#reactList2<-reactList[reactList$cpd==" I s1021 I s1022 I p835",]
insertEdges <- function(reactList2){
  cat("Inserting edges [",counter,"of",total,"]\n")
  counter<<-counter+1
  reactList2<-reactList2[-2]#$dup_count<-NULL
  reactList2<-as.vector((reactList2[!is.na(reactList2)]))
  #cat(reactList2,'\n')
  idx<-3
  #nId is the reaction database identifier
  nId<-as.numeric(trim(reactList2[2]))
  #list all reactions
  reacts<-''
  if(length(reactList2)>2){
    for (idx in 2:(length(reactList2)-1)) {
      reacts<-paste0(reacts,as.numeric(trim(reactList2[idx])),',')
    }
    reacts<-paste0(reacts,as.numeric(trim(reactList2[idx+1])))
  }else{
    reacts<-paste0(reacts,as.numeric(trim(reactList2[2])))
  }
    
  # sql <- paste0('select rName
  #               from reaction
  #               where rId = ',nId)
  # 
  # rName<- substring(dbGetQuery(dbCon,sql)[1,1],4)
  #get enzime names
  sql <- paste0('SELECT e.eId, e.eName
                FROM enzime as e INNER JOIN
                    enzReac as er on er.eId = e.eId
                WHERE er.rId in (', reacts,')
                order by eName')
  eNames<- dbGetQuery(dbCon,sql)
  if(nrow(eNames) == 0 ){
    cat(file = logFile,'error in ',nId, reactList2,'\n',append = T)
  }
  nName <- eNames[1,2]
  
  sql <- paste0('SELECT DISTINCT rId, rName
                  FROM 
                      (SELECT r.rId, r.rName
                      FROM reaction as r
                      WHERE rId in (', reacts,')
                      UNION
                      SELECT r.rId, r.rName
                      FROM reaction as r INNER JOIN
                          reactionAssociation as ra on ra.rId = r.rId 
                      WHERE ra.mainRId in (', reacts,'))
                  order by rName')
  
  rNames<- dbGetQuery(dbCon,sql)
  rName <- rNames[1,2]
  # fName <- paste0('f',
  #                 substring(
  #                   gsub(pattern = '[.]',
  #                        replacement = '',
  #                        nName),4) )
  # set new edge name 
  if(nrow(eNames)>1){
    nName<-paste0(nName,"+")
  }
  repName<-checkNewName(name = nName,
                        table = 'edges',
                       type = 'n')
  if(repName > 0){
    nName <- paste0(nName,"_",repName)
  }

  # set new reaction name 
  if(nrow(rNames)>1){
    rName<-paste0(rName,"+")
  }
  repName<-checkNewName(name = rName,
                        table = 'edges',
                        type = 'r')
  if(repName > 0){
    rName <- paste0(rName,"_",repName)
  }
  #insert aliases
  idx=1
  for(idx in 1:nrow(eNames)){
    sql<-paste0('INSERT INTO nodeAlias
                VALUES (',
                nId,',',
                eNames[idx,1],
                ',"e")')
    resQuery <- dbExecute(dbCon,sql)
    
  }
  for(idx in 1:nrow(rNames)){
    sql<-paste0('INSERT INTO nodeAlias
                VALUES (',
                nId,',',
                rNames[idx,1],
                ',"r")')
    resQuery <- dbExecute(dbCon,sql)
  }
  fName <- paste0('f', nName)
  
  reversib <- substr(trim(reactList2[1]),1,1)
  compounds <- gsub(pattern = paste0(' ',reversib,' '),
                   replacement = ':',
                   x = reactList2[1])
  # is the reaction reversible?
  isRevers <- ifelse(reversib == 'I', 0, 1)
  compounds <- unlist(strsplit(compounds, split = ':'))[-1]
  #vectors for substrate and product
  subs <- vector()
  prod <- vector()
  #separete subs an prod
  for(idx in 1: length(compounds)){
    if(substr(compounds[idx],1,1) == "s"){
      subs <- c(subs, substring(compounds[idx],2))
    }else{
      prod <- c(prod, substring(compounds[idx],2))
    }
  }
  edgeList <- list()
  idxEdge <- 1
  fakeIdS <- 0
  fakeIdP <- 0
  #create fake edges to agrupate multiple compound
  idx2=1
  if(length(subs) > 1){
    fakeIdS<- getNextFakeEdge()
    for(idx2 in 1:length(subs)){
      fakeNameS <- paste0(fName,'S',idx2)
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = fakeNameS,
                                      rName = rName,
                                      subs=as.numeric(subs[idx2]),
                                      prod=fakeIdS,
                                      type = "F",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
      idxEdge <- idxEdge +1
      if(isRevers == 1){
        edgeList[[idxEdge]]<-data.frame(nId = nId,
                                        nName = fakeNameS,
                                        rName = rName,
                                        subs=fakeIdS,
                                        prod=as.numeric(subs[idx2]),
                                        type = "F",
                                        reversible = isRevers,
                                        stringsAsFactors = F)
      }
        idxEdge <- idxEdge +1
    }
  }
  if(length(prod) > 1){
    fakeIdP<- getNextFakeEdge()
    for(idx2 in 1:length(prod)){
      fakeNameP <- paste0(fName,'P',idx2)
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = fakeNameP,
                                      rName = rName,
                                      subs=fakeIdP,
                                      prod=as.numeric(prod[idx2]),
                                      type = "F",
                                      reversible = isRevers,
                                      stringsAsFactors = F)

      idxEdge <- idxEdge +1
      if(isRevers == 1){
        edgeList[[idxEdge]]<-data.frame(nId = nId,
                                        nName = fakeNameP,
                                        rName = rName,
                                        subs= as.numeric(prod[idx2]),
                                        prod= fakeIdP,
                                        type = "F",
                                        reversible = isRevers,
                                        stringsAsFactors = F)
      }
      idxEdge <- idxEdge +1
    }
  }
  #Create the real edge
  #add edge if both fake
  if(fakeIdS !=0 & fakeIdP !=0){
    edgeList[[idxEdge]]<-data.frame(nId = nId,
                                    nName = fakeNameP,
                                    rName = rName,
                                    subs=fakeIdS,
                                    prod=fakeIdP,
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = fakeNameP,
                                      rName = rName,
                                      subs= fakeIdP,
                                      prod= fakeIdS,
                                      type = "R",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
    }
    idxEdge <- idxEdge +1
  }
  #add edge if S is fake
  if(fakeIdS !=0 & fakeIdP ==0){
    edgeList[[idxEdge]]<-data.frame(nId = nId,
                                    nName = nName,
                                    rName = rName,
                                    subs=fakeIdS,
                                    prod= prod[1],
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
                                      rName = rName,
                                      subs= prod[1],
                                      prod= fakeIdS,
                                      type = "R",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
    }
    idxEdge <- idxEdge +1
  }
  #add edge if P is fake
  if(fakeIdS ==0 & fakeIdP !=0){
    edgeList[[idxEdge]]<-data.frame(nId = nId,
                                    nName = nName,
                                    rName = rName,
                                    subs = subs[1],
                                    prod=fakeIdP,
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
                                      rName = rName,
                                      subs= fakeIdP,
                                      prod=  subs[1],
                                      type = "R",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
    }
    idxEdge <- idxEdge +1
  }
  #add edge if both are not fake
  if(fakeIdS ==0 & fakeIdP ==0){
    edgeList[[idxEdge]]<-data.frame(nId = nId,
                                    nName = nName,
                                    rName = rName,
                                    subs=subs[1],
                                    prod=prod[1],
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
                                      rName = rName,
                                      subs= prod[1],
                                      prod= subs[1],
                                      type = "R",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
    }
    idxEdge <- idxEdge +1
  }
  
  edgeList <- do.call(rbind, edgeList)
  
  #create fake node information
  if(fakeIdS !=0){
    sql <- paste0(
      'INSERT INTO fakeNode
                  VALUES (',
          fakeIdS,',"',
          fakeNameS, '","");'
    )
    resQuery <- dbExecute(dbCon,sql)
  }
    
  if(fakeIdP !=0){
    sql <- paste0(
      'INSERT INTO fakeNode
                  VALUES (',
      fakeIdP,',"',
      fakeNameP, '","");'
    )
    resQuery <- dbExecute(dbCon,sql)
    
  }
  lixo<- apply(X = edgeList, MARGIN = 1,
        FUN = function(x){
          sql <- paste0(
                'INSERT INTO edges
                  VALUES (',
                  x["nId"],',"',
                  x["nName"], '","',
                  x["rName"], '",',
                  x["subs"],',',
                  x["prod"],',"',
                  x["type"],'",',
                  x["reversible"],');'
                  )
          resQuery <- dbExecute(dbCon,sql)
          return(0)
        })
  return(0)
}





keggErrorsFix <- function(){
  #correct some erros found on KEGG xml where
  # some reactions have inconsistences from one path to another
  
  #correct reactions that have different reversibility assingn
  # will be consider correct all reversible reactions
  # irreversible reactions will be deleted
  # sql<-'SELECT * 
  #       FROM reaction
  #       WHERE rName in (
  #       		SELECT rName
  #       		FROM reaction
  #       		GROUP by rName
  #       		HAVING count(*) > 1)
  #       ORDER by rName'
  # react<-dbGetQuery(dbCon,sql)
  # tmp<-react[react$rReversible == 0,c("rId","rName")]
  # colnames(tmp)[1]<-"oldId"
  # react<-react[react$rReversible == 1,]
  # react<- merge(react,tmp, by="rName")
  # #correct the rId in every table where its appears
  # tables <- c("subsProd","reaction")
  # idx=1
  # for(idx in 1:nrow(react)){
  #   newId <- react$rId[idx]
  #   oldId <- react$oldId[idx]
  #   #update tables
  #   sql <- paste0('UPDATE enzReac',
  #                 ' set rId = ', newId,
  #                 ' where rid = ', oldId,';')
  #   resQuery <- dbExecute(dbCon,sql)
  #   sql <- paste0('UPDATE reacOnPath',
  #                 ' set rId = ', newId,
  #                 ' where rid = ', oldId,';')
  #   resQuery <- dbExecute(dbCon,sql)
  #   sql <- paste0('UPDATE reactionAssociation',
  #                 ' set rId = ', newId,
  #                 ' where rid = ', oldId,';')
  #   resQuery <- dbExecute(dbCon,sql)
  #   sql <- paste0('UPDATE reactionAssociation',
  #                 ' set mainRId = ', newId,
  #                 ' where mainRId = ', oldId,';')
  #   resQuery <- dbExecute(dbCon,sql)
  #   #delete from tables
  #   for(table in tables){
  #     sql <- paste0('DELETE FROM ',
  #                   table,
  #                   ' where rid = ', oldId,';')
  #     resQuery <- dbExecute(dbCon,sql)
  #   }
  #   
  # }
  
  #Correct substract product duplicity and inconsistences
  sql<-'select distinct rId, rName
        from reaction'
  react<-dbGetQuery(dbCon,sql)
  #manual solutions
  sql<-'select distinct cId, cName
        from compound
        where cName in ("cpd:C00022","cpd:C00024", "cpd:C00084","cpd:C03589",
                        "cpd:C00048","cpd:C00100","cpd:C06027","cpd:C00036",
                        "cpd:C00158","cpd:C00010")'
  cpd<-dbGetQuery(dbCon,sql)
  errors<-list()
  idx=1744 #debug
  counter = 0
  errorlist<- list()
  for(idx in 1:nrow(react)){
    sql<-paste0('select cId
                from subsProd
                where spType = "s" and
                      secondary = 0 and
                      rId = ',react$rId[idx])
    subs <- sort(as.vector(dbGetQuery(dbCon,sql)[,1]))
    
    sql<-paste0('select cId
                from subsProd
                where spType = "p" and
                      secondary = 0 and
                      rId = ',react$rId[idx])
    prod <- sort(as.vector(dbGetQuery(dbCon,sql)[,1]))
    
    if(sum(prod %in% subs) == length(prod)&
       length(prod)>0){
      counter <-counter+1
      errorlist[[counter]]<- data.frame(name = react$rName[idx],
                                        id = react$rId[idx],
                                        subs=length(subs),
                                        prod=length(prod),
                                        stringsAsFactors = F)
      cat(counter,": Error in ", 
          react$rName[idx],
          length(prod),
          '. Correcting...\n')
      cat(file = logFile, append = T,
          counter,": Error in ", 
          react$rName[idx],
          react$rId[idx],
          length(subs),
          length(prod),'.Correcting...\n')
      #remove first prod and last subs
      if(length(prod) == 2 &
         length(subs) == 2){
        sql<- paste0('delete from subsProd
                      where spType = "s" and
                            secondary = 0 and
                            rId = ',react$rId[idx], ' and 
                            cId = ', subs[1])
        resQuery <- dbExecute(dbCon,sql) 
        sql<- paste0('delete from subsProd
                      where spType = "p" and
                            secondary = 0 and
                            rId = ',react$rId[idx], ' and 
                            cId = ', prod[2])
        resQuery <- dbExecute(dbCon,sql) 
      }else{
        #manual solutions
        if(react$rName[idx] == 'rn:R00750'){
          #bases on ec00360
          sql<- paste0('delete from subsProd
                      where spType = "p" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and 
                            cId = ', cpd$cId[cpd$cName == "cpd:C00022"])
          resQuery <- dbExecute(dbCon,sql) 
          sql<- paste0('delete from subsProd
                      where spType = "p" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and  
                            cId = ', cpd$cId[cpd$cName == "cpd:C00084"])
          resQuery <- dbExecute(dbCon,sql) 
          sql<- paste0('delete from subsProd
                      where spType = "s" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and 
                            cId = ', cpd$cId[cpd$cName == "cpd:C03589"])
          resQuery <- dbExecute(dbCon,sql) 
          
        }else if(react$rName[idx] == 'rn:R00934'){
          #bases on ec00630
          sql<- paste0('delete from subsProd
                      where spType = "p" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and 
                            cId = ', cpd$cId[cpd$cName == "cpd:C06027"])
          resQuery <- dbExecute(dbCon,sql) 
          sql<- paste0('delete from subsProd
                      where spType = "s" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and  
                            cId = ', cpd$cId[cpd$cName == "cpd:C00048"])
          resQuery <- dbExecute(dbCon,sql) 
          sql<- paste0('delete from subsProd
                      where spType = "s" and
                            secondary = 0 and
                            rId = ',react$rId[idx],' and 
                            cId = ', cpd$cId[cpd$cName == "cpd:C00100"])
          resQuery <- dbExecute(dbCon,sql) 
          
        }
      }
    }
  }
  # manual correctios of reaction R00351
  # bases on ec00020 and reaction definition
  # https://www.kegg.jp/dbget-bin/www_bget?rn:R00351
  sql<- paste0('delete from subsProd
                      where rId = ',react$rId[react$rName == "rn:R00351"])
  resQuery <- dbExecute(dbCon,sql)
  db <- data.frame(names =  c("cpd:C00010","cpd:C00158",
                              "cpd:C00036","cpd:C00024"),
                   type = c("s","s","p","p"),
                   stringsAsFactors = F) 
  idx = 1
  for(idx in 1:4){
    sql<- paste0('insert into subsProd
          VALUES (',react$rId[react$rName == "rn:R00351"],',',
          cpd$cId[cpd$cName == db$names[idx]],',"',
          db$type[idx],'",',
          '0)')
    resQuery <- dbExecute(dbCon,sql) 
  }
  
  #enzymes reactions erros
  sql<- 'INSERT INTO enzReac VALUES(3, 2825);'
  resQuery <- dbExecute(dbCon,sql) 
  sql<- 'INSERT INTO enzReac VALUES(123, 183);'
  resQuery <- dbExecute(dbCon,sql) 
  sql<- 'INSERT INTO enzReac VALUES(345, 471);'
  resQuery <- dbExecute(dbCon,sql) 
  
  
  errorlist<-do.call(rbind, errorlist) #debug
  
  error3<- errorlist[errorlist$subs >2 | 
                       errorlist$prod >2,]#debug
  errorlist<-errorlist[errorlist$subs <=2 & 
                         errorlist$prod <=2,]#debug
  
  
  
}

setSecondaryCompounds <- function(){
  # assingn secondary compounds
  sql<- "update subsProd set secondary = 0;"
  resQuery <- dbExecute(dbCon,sql)
  sql<-'update subsProd set secondary = 1
        where cId in(
              select cid 
              from compound 
              where cName in ("cpd:C00001","cpd:C00002","cpd:C00008","cpd:C00011",
                              "cpd:C00014","cpd:C00059","cpd:C00066","cpd:C00080","cpd:C00086",
                              "cpd:C00088","cpd:C00094","cpd:C00288","cpd:C00533","cpd:C01322",
                              "cpd:C01371","cpd:C01438","cpd:C01528","cpd:C06049","cpd:C09306",
                              "cpd:C11481","cpd:C14818","cpd:C00009")
            );'
  resQuery <- dbExecute(dbCon,sql)
  
}


getEdgesFromEcs <- function(ecs,
                     pathway){
  #retrive edges from a pathway graph using
  # enzime ecs identification
  
  pathway<- paste0('"',pathway,'"')
  ecs<- paste0('"',ecs,'"')
  ecs <- do.call(paste, c(as.list(ecs), sep = ","))
  sql <- paste0(
  'SELECT c1.cName as "from", c2.cName as "to", e.nName as "eName", e.type
  FROM edges as e INNER JOIN
  	reaction as r on r.rId = e.nId INNER JOIN
  	wAllNodes as c1 on c1.cId = e.subs INNER JOIN
  	wAllNodes as c2 on c2.cId = e.prod
  WHERE nId in (SELECT mainRId
  			FROM reactionAssociation
  			where rId in (SELECT r.rId
  						from reaction as r inner JOIN
  							enzReac as er on er.rId = r.rId INNER JOIN
  							enzime as e on e.eId = er.eId INNER JOIN
  							reacOnPath as ep on ep.rId = r.rId INNER JOIN
  							path as p on p.pId = ep.pId
  						where eName in (',ecs,') AND
  							pName = ',pathway,'))')
  sql <- gsub(pattern = '\t',replacement = '',sql)
  sql <- gsub(pattern = '\n',replacement = '',sql)
  sql
  edges <- dbGetQuery(dbCon,sql)
  
  edges$from<-sub(pattern = 'cpd:',
                  replacement = '',
                  edges$from)
  edges$to<-sub(pattern = 'cpd:',
                  replacement = '',
                  edges$to)
  edges <- unique(edges) 
  return(edges)
}

getEdgesFromPath <- function(pathway, org = NA){
  #retrive edges from a pathway graph using
  # enzime ecs identification
  
  sql<-paste0('SELECT COUNT(*)
              FROM path 
              WHERE pName = "',pathway,'";')
  count<- dbGetQuery(dbCon,sql)[1,1]
  if(count == 0){
    stop(paste0('Pathway ',pathway,' not found'))
  }
  pathway<- paste0('"',pathway,'"')
  # ecs<- paste0('"',ecs,'"')
  # ecs <- do.call(paste, c(as.list(ecs), sep = ","))
  if(is.na(org)){
    sql <- paste0(
      'SELECT c1.cName as "from", 
            c2.cName as "to",
            e.nId as "nId",
            e.rName as "rName",
            n.eName as "eName", 
            e.type  
      FROM edges as e INNER JOIN 
        nodes as n on n.nId = e.nId INNER JOIN
      	reaction as r on r.rId = e.nId INNER JOIN  
      	wAllNodes as c1 on c1.cId = e.subs INNER JOIN  
      	wAllNodes as c2 on c2.cId = e.prod  
      WHERE e.nId in (
      	SELECT mainRId  
      	FROM reactionAssociation  
      	where rId in (
      		SELECT r.rId  from reaction as r inner JOIN
      		reacOnPath as ep on ep.rId = r.rId INNER JOIN  
      		path as p on p.pId = ep.pId  
      		where pName = ',pathway,'))')
  }else{
    org<- paste0('"',org,'"')
    sql <- paste0(
      'SELECT c1.cName as "from", 
    c2.cName as "to",
    e.nId as "nId",
    e.rName as "rName",
    n.eName as "eName", 
    e.type 
    FROM edges as e INNER JOIN  
    nodes as n on n.nId = e.nId INNER JOIN
    reaction as r on r.rId = e.nId INNER JOIN  
    wAllNodes as c1 on c1.cId = e.subs INNER JOIN  
    wAllNodes as c2 on c2.cId = e.prod  
    WHERE n.nId in (
      SELECT nId  
      FROM nodebyorgs as nog INNER JOIN  
      path as p on p.pId = nog.pId  
      WHERE pName = ',pathway,' AND
      org = ',org,')')
  }
  # e.nName as "eName", #replaced
  # nodes as n on n.nId = e.nId INNER JOIN #added
  # WHERE e.nId in ( # add e.
  
  sql <- gsub(pattern = '\t',replacement = '',sql)
  sql <- gsub(pattern = '\n',replacement = '',sql)
  sql
  edges <- dbGetQuery(dbCon,sql)
  
  edges$from<-sub(pattern = 'cpd:',
                  replacement = '',
                  edges$from)
  edges$to<-sub(pattern = 'cpd:',
                replacement = '',
                edges$to)
  edges <- unique(edges) 
  return(edges)
}

getGraphFromPath<-function(pathway,
                           removeFake = T,
                           auxInfo = T,
                           org = NA){
  closeDb <<- F
  createDbConnection()
  #get edges from path
  edges<-getEdgesFromPath(pathway = pathway, org)
  
  # Handle the case when the pathway doesn't exists
  if (is.null(edges) || nrow(edges) == 0) {
    return(NULL)
  }
  
  #get coord of compounds for plot
  sql<-paste0('select cName, x, y
              FROM compOnPath as cp INNER JOIN
              	compound as c on c.cId = cp.cId inner JOIN
              	path as p on p.pid = cp.pId
              WHERE p.pName = "',pathway,'"')
  cpd<- dbGetQuery(dbCon,sql)
  cpd$cName<-sub(pattern = 'cpd:',replacement = '',
                 cpd$cName)
  cpd<-cpd[!duplicated(cpd$cName),]
  
  #create the compound graph
  g1 <- graph_from_data_frame(edges, 
                              directed=TRUE, 
                              vertices=NULL)
  
  tmp<-data.frame(idx = seq(1,length(V(g1))),
                  cName = V(g1)$name,
                  stringsAsFactors = F)
  
  cpd<-merge(tmp,cpd,by="cName",all.x = T)
  cpd<-cpd[order(cpd$idx),]
  cpd$x[is.na(cpd$x)]<-600
  cpd$y[is.na(cpd$y)]<-300
  
  cpd$x <- (cpd$x-min(cpd$x))/
    (max(cpd$x)-min(cpd$x))*1000 + 25
  cpd$y <- (cpd$y-min(cpd$y))/
    (max(cpd$y)-min(cpd$y))*500 + 25
  

  attrs<-data.frame(color='cyan',
                    name = V(g1)$name,
                    curved = T,
                    stringsAsFactors = F)
  attrs$color[grep('^f',attrs$name)]<-"red"
  
  vertex_attr(g1) <- list(name= attrs$name,
                          color = attrs$color,
                          curved = attrs$curved,
                          x= cpd$x,
                          y = cpd$y)
  
  edge_attr(g1, "curved") <- rep(T, gsize(g1))
  
  g4 <- NULL
  
  tryCatch({
    g4<-cleanedLineGraph(g1, removeFake = removeFake)
  }, error=function(e) {})
  
  if (is.null(g4)) {
    return(NULL)
  }
  
  attrs<-data.frame(idx = seq(1,length(V(g4))),
                    color='yellow',
                    eName = vertex_attr(g4,"eName"),
                    stringsAsFactors = F)
  attrs$eName <- gsub(pattern = "[+]",
                      replacement = '',
                      attrs$eName)
  attrs$eName <- gsub(pattern = "_[0-9]",
                      replacement = '',
                      attrs$eName)
  #get enzymes coord
  sql<-paste0('select eName, x, y
              FROM enzOnPath as ep INNER JOIN
                enzime as e on e.eId = ep.eId inner JOIN
                path as p on p.pid = ep.pId
              WHERE p.pName ="',pathway,'"') 
  cpd<- dbGetQuery(dbCon,sql)
  
  cpd$x[is.na(cpd$x)]<-600
  cpd$y[is.na(cpd$y)]<-300
  
  cpd$x <- (cpd$x-min(cpd$x))/
    (max(cpd$x)-min(cpd$x))*1000 + 25
  cpd$y <- (cpd$y-min(cpd$y))/
    (max(cpd$y)-min(cpd$y))*500 + 25
  
  attrs<-merge(attrs,cpd,
               by.x='eName',
               by.y = 'eName',
               all.x = T)

  attrs<-attrs[order(attrs$idx),]

  vertex_attr(g4, "color") <- attrs$color
  vertex_attr(g4, "x") <- attrs$x
  vertex_attr(g4, "y") <- attrs$y
  
  if(closeDb){
    dbDisconnect(dbCon)  
  }
  
  edge_attr(g1,'label')<-edge_attr(g1,"nId")
  vertex_attr(g4,'name')<-vertex_attr(g4,"nId")

  if(auxInfo){
    return(list(g1,g4))
  }else{
    return(g4)
  }

}

showGraph<-function(pathway,
                    auxInfo = T,
                    label = 'enzyme',
                    removeFake = T,
                    org = NA){

  if(!label %in% c('enzyme','reaction','id')){
    stop('Label must be "enzyme", "reaction" or "id".')
  }
  
  lGraph<-getGraphFromPath(pathway = pathway,
                           removeFake = removeFake,
                           auxInfo = auxInfo,
                           org = org)
  g1<-lGraph[[1]]
  g2 <-lGraph[[2]]
  # coords1<-lGraph[[3]]
  # coords2<-lGraph[[4]]
  
  if(label == 'enzyme'){
    edge_attr(g1,'label')<-edge_attr(g1,"eName")
    vertex_attr(g2,'name')<-vertex_attr(g2,"eName")
  }else if(label == 'reaction'){
    edge_attr(g1,'label')<-edge_attr(g1,"rName")
    vertex_attr(g2,'name')<-vertex_attr(g2,"rName")
  }
  coords1 <- matrix(c(V(g1)$x,V(g1)$y),ncol = 2)
  coords2 <- matrix(c(V(g2)$x,V(g2)$y),ncol = 2)
  
  tk1<-tkplot(g1,canvas.width = 1200,
              canvas.height = 650)
  tk_set_coords(tk1,coords1)
  tk_center(tk1)

  tk2<-tkplot(g2,canvas.width = 1200,
              canvas.height = 650)
  tk_set_coords(tk2,coords2)
  tk_center(tk2)

}


showGraphOld<-function(ecs = NA, 
                    pathway,
                    adj = T,
                    plot = T,
                    removeFake = F){
  
  dbDir<<-file.path(dirBase,"data","database")
  dbFile<<-file.path(dbDir,"dictionary.db")
  #conect and test dictionary
  dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
  
  #pathway <- "ec00010"
  
  if(is.na(ecs)){
    edges<-getEdgesFromPath(pathway = pathway)
  }else{
    # ecs<-c('ec:5.1.3.3','ec:2.7.1.2','ec:2.7.1.147',
    #        'ec:5.1.3.15','ec:5.3.1.9','ec:2.7.1.199',
    #        'ec:2.7.1.63','ec::2.7.1.1','ec:3.1.3.10',
    #        'ec:3.1.3.9','ec:5.4.2.2')
    #
    # ecs<-c('ec:2.3.1.12','ec:1.2.4.1','ec:1.8.1.4',
    #        'ec:1.2.7.1','ec:1.2.7.11','ec:2.7.1.40',
    #        'ec:4.1.1.1','ec:4.2.1.11')
    #
    
    edges <- getEdgesFromEcs(ecs = ecs, pathway = pathway )
  }
  sql<-paste0('select cName, x, y
              FROM compOnPath as cp INNER JOIN
              	compound as c on c.cId = cp.cId inner JOIN
              	path as p on p.pid = cp.pId
              WHERE p.pName = "',pathway,'"')
  cpd<- dbGetQuery(dbCon,sql)
  cpd$cName<-sub(pattern = 'cpd:',replacement = '',
                 cpd$cName)
  cpd<-cpd[!duplicated(cpd$cName),]
  
  
  g1 <- graph_from_data_frame(edges,
                              directed=TRUE,
                              vertices=NULL)
  
  tmp<-data.frame(idx = seq(1,length(V(g1))),
                  cName = V(g1)$name,
                  stringsAsFactors = F)
  
  cpd<-merge(tmp,cpd,by="cName",all.x = T)
  cpd<-cpd[order(cpd$idx),]
  cpd$x[is.na(cpd$x)]<-600
  cpd$y[is.na(cpd$y)]<-300
  
  cpd$x <- (cpd$x-min(cpd$x))/
    (max(cpd$x)-min(cpd$x))*1000 + 25
  cpd$y <- (cpd$y-min(cpd$y))/
    (max(cpd$y)-min(cpd$y))*500 + 25
  coords<-matrix(c(cpd$x,cpd$y),ncol = 2)
  
  edgeNames<-E(g1)$eName
  edge_attr(g1)
  #print(g1, e=TRUE, v=TRUE)
  edge_attr(g1) <- list(color = rep("black", gsize(g1)),
                        curved = rep(T, gsize(g1)))
  edge_attr(g1, "label") <- edgeNames
  #tkplot(g1)
  
  attrs<-data.frame(color='cyan',
                    name = V(g1)$name,
                    stringsAsFactors = F)
  attrs$color[grep('^f',attrs$name)]<-"red"
  
  vertex_attr(g1) <- list(name= attrs$name,
                          color = attrs$color)
  tk1<-tkplot(g1,canvas.width = 1200,
              canvas.height = 650)
  tk_set_coords(tk1,coords)
  tk_center(tk1)
  
  g3<-cleanedLineGraphOld(g1, removeFake = removeFake)
  
  
  g4<-graph_from_data_frame(g3, directed = T)
  
  attrs<-data.frame(color='yellow',
                    name = V(g4)$name,
                    stringsAsFactors = F)
  attrs$color[grep('fec:',attrs$name)]<-"red"
  vertex_attr(g4) <- list(name= attrs$name,
                          color = attrs$color)
  sql<-paste0('select eName, x, y
              FROM enzOnPath as ep INNER JOIN
                enzime as e on e.eId = ep.eId inner JOIN
                path as p on p.pid = ep.pId
              WHERE p.pName ="',pathway,'"')
  cpd<- dbGetQuery(dbCon,sql)
  # cpd$eName<-sub(pattern = 'cpd:',replacement = '',
  #                cpd$cName)
  # cpd<-cpd[!duplicated(cpd$eName),]
  
  tmp<-data.frame(idx = seq(1,length(V(g4))),
                  eName = V(g4)$name,
                  stringsAsFactors = F)
  tmp$eName<-sub(pattern = '[+]',replacement = '',
                 tmp$eName)
  cpd<-merge(tmp,cpd,by="eName",all.x = T)
  cpd<-cpd[order(cpd$idx),]
  cpd$x[is.na(cpd$x)]<-600
  cpd$y[is.na(cpd$y)]<-300
  
  cpd$x <- (cpd$x-min(cpd$x))/
    (max(cpd$x)-min(cpd$x))*1000 + 25
  cpd$y <- (cpd$y-min(cpd$y))/
    (max(cpd$y)-min(cpd$y))*500 + 25
  coords<-matrix(c(cpd$x,cpd$y),ncol = 2)
  
  tk4<-tkplot(g4,canvas.width = 1200,
              canvas.height = 650)
  tk_set_coords(tk4,coords)
  tk_center(tk4)
  
  dbDisconnect(dbCon)
  return(g4)
  # 
  #plot(edges)
}

cleanedLineGraph <- function(g1, removeFake = F){
  
  #labels<-edge_attr(g1,"label")
  nIds<-edge_attr(g1,"nId")
  rNames <- edge_attr(g1,"rName")
  eNames <- edge_attr(g1,"eName")
  gData <- data.frame(nId = nIds,
                      rName = rNames,
                      eName = eNames,
                      stringsAsFactors = F)
  
#  edgeNames<-E(g1)$label
  g2 <- make_line_graph(g1)
  
  #vertex_attr(g2, "label")<- labels
  vertex_attr(g2, "nId")<-nIds
  vertex_attr(g2, "rNames") <- rNames
  vertex_attr(g2, "eNames") <- eNames
  vertex_attr(g2,"name")<-nIds
  
  #vNames<- V(g2)$label
  vNames<- data.frame(nr = seq(1,length(V(g2)),1), 
                      nId = nIds,
                      rNames = rNames,
                      eNames = eNames,
                      stringsAsFactors = F)
  
  #remove duplicity
  g3<-igraph::as_data_frame(g2,what = "edges")
  
  nrow(g3[g3$from == g3$to,])
  sum(duplicated(vNames$label))
  
  g3<- merge(g3, vNames[,c("nId","eNames","rNames")], 
             by.x="from",
             by.y="nId")
  colnames(g3) <- c("fromId","toId","from","fromR")
  
  g3<- merge(g3, vNames[,c("nId","eNames","rNames")], 
             by.x="toId",
             by.y="nId")
  # g3$toO<-NULL
  # g3$fromO<-NULL
  colnames(g3)[5] <- "to"
  colnames(g3)[6] <- "toR"
  
  if(removeFake){
    #remove fake nodes
    g3$from<-sub(pattern = 'S.+$', 
                 replacement = '', 
                 sub(pattern = '^f',replacement = '', 
                     g3$from))
    
    g3$to<-sub(pattern = 'S.+$', 
               replacement = '', 
               sub(pattern = '^f',replacement = '', 
                   g3$to))
    g3$from<-sub(pattern = 'P.+$', 
               replacement = '', 
               sub(pattern = '^f',replacement = '', 
                   g3$from))
    g3$to<-sub(pattern = 'P.+$', 
               replacement = '', 
               sub(pattern = '^f',replacement = '', 
                   g3$to))
  }
  #remove duplicated
  g3<-g3[g3$from != g3$to,]
  g3<-g3[!duplicated(g3),]
  
  tmp1<-g3[,c("fromId","from","fromR")]
  colnames(tmp1)<-c("nId","eName", "rName")
  tmp2<-g3[,c("toId","to","toR")]
  colnames(tmp2)<-c("nId","eName", "rName")
  dictionary<-rbind(tmp1,tmp2)
  dictionary<-dictionary[!duplicated(dictionary),]
  
  
  g3<-g3[,c("fromId","toId")]
  colnames(g3)<-c("from","to")

  
  g4<-graph_from_data_frame(g3, directed = T)
  tmp1<- data.frame(nr = seq(1,length(V(g4)),1), 
                      nId = V(g4)$name)
  dictionary<-merge(dictionary,
                    tmp1,
                    by="nId")
  
  dictionary<-dictionary[order(dictionary$nr),]
      
  vertex_attr(g4, "nId")<-dictionary$nId
  vertex_attr(g4, "rName") <- dictionary$rName
  vertex_attr(g4, "eName") <- dictionary$eName
  V(g4)
  return(g4)
  
}

cleanedLineGraphOld <- function(g1, removeFake = F){
  
  
  edgeNames<-E(g1)$label
  g2 <- make_line_graph(g1)
  
  vertex_attr(g2, "label")<- edgeNames
  
  vNames<- V(g2)$label
  vNames<- data.frame(nr = seq(1,length(vNames),1), 
                      label = vNames,
                      stringsAsFactors = F)
  
  #remove duplicity
  g3<-as_data_frame(g2,what = "edges")
  
  nrow(g3[g3$from == g3$to,])
  sum(duplicated(vNames$label))
  
  g3<- merge(g3, vNames, 
             by.x="from",
             by.y="nr")
  colnames(g3) <- c("fromO","toO","from")
  
  g3<- merge(g3, vNames, 
             by.x="toO",
             by.y="nr")
  g3$toO<-NULL
  g3$fromO<-NULL
  colnames(g3) <- c("from","to")
  
  if(removeFake){
    #remove fake nodes
    g3$from<-sub(pattern = 'S.+$', 
                 replacement = '', 
                 sub(pattern = '^f',replacement = '', 
                     g3$from))
    
    g3$to<-sub(pattern = 'S.+$', 
               replacement = '', 
               sub(pattern = '^f',replacement = '', 
                   g3$to))
    g3$from<-sub(pattern = 'P.+$', 
                 replacement = '', 
                 sub(pattern = '^f',replacement = '', 
                     g3$from))
    g3$to<-sub(pattern = 'P.+$', 
               replacement = '', 
               sub(pattern = '^f',replacement = '', 
                   g3$to))
  }
  #remove duplicated
  g3<-g3[g3$from != g3$to,]
  g3<-g3[!duplicated(g3),]
  
  return(g3)
  
}


removeFakeNodes<-function(){
  
}

checkNewName <- function(name,
                         table,
                         type){
  if(table == 'edges'){
    if(!type %in% c('n','r')){
      stop('Use "n" for node name, and "r" for reaction name')
    }
    sql <- paste0('SELECT COUNT(*) as qtd
          FROM ',table,
                  ' WHERE ',type,'Name = "', name,'";')
  }else if (table == 'nodes'){
    if(!type %in% c('e','r')){
      stop('Use "e" for enzyme name, and "r" for reaction name')
    }
    sql <- paste0('SELECT COUNT(*) as qtd
          FROM ',table,
          ' WHERE ',type,'Name = "', name,'";')
    
  }
  return(dbGetQuery(dbCon,sql)[1,1])
}

getAllPathways <- function(){
  sql<-paste0('SELECT pName
              FROM path;')
  paths<- dbGetQuery(dbCon,sql)[,1]
  return(paths)
}

insertMetrics <- function(pathway){
  #pathway = "ec00010" #debug
  pId <- getPIdFromName(pathway = pathway )
  g<-getGraphFromPath(pathway = pathway, 
                      removeFake = T,
                      auxInfo =F )
  gProp<-getGraphProperties(g)
  gProp$isAP<-0
  APs<-as.character(getGraphBottleneck(g))
  if(any(!APs %in% gProp$node)){
    stop('At least one AP is missing in ',pathway)
  }
  gProp$isAP[gProp$node %in% APs] <- 1
  
  #for cases where no metric is avaliable
  gProp[is.na(gProp)]<- -1

  #insert df lines on table
  idx=1
  for (idx in 1:nrow(gProp)) {
    sql <- paste0('INSERT INTO nodeMetric VALUES(',
                  gProp$node[idx], ',', 
                  pId, ',',
                  gProp$isAP[idx], ',',
                  gProp$connectivity[idx], ',',
                  gProp$triangles[idx], ',',
                  gProp$community[idx], ',',
                  gProp$eccentricity[idx], ',',
                  gProp$radius[idx], ',',
                  gProp$diameter[idx], ',',
                  gProp$degree[idx], ',',
                  gProp$betweenness[idx], ',',
                  gProp$clusteringCoef[idx], ',',
                  gProp$closenessCoef[idx], ',',
                  gProp$eigenvectorScore[idx], ',',
                  gProp$authorityScore[idx], ',',
                  gProp$hubScore[idx], ');')
    resQuery <- dbExecute(dbCon,sql)
    
  }
                
                
}

cleanMetrics<- function(){
  #prepare the table nodeMetric to receive data
  sql <- "DELETE FROM nodeMetric"
  resQuery <- dbExecute(dbCon,sql)
  
}

getNodeMetrics <- function(nodeIds_, pathwayId_) {
  # DB attributes
  table <- "nodemetric"
  fields <- c("nId", "pId")
  
  values<-list(nodeIds_, pathwayId_)
  
  # Search in DB
  resQuery <- searchValue(table, fields, values)
  
  # Check if at least one pathway was returned
  if (is.null(resQuery) || length(resQuery) == 0) {
    stop("Nodes not found!")
  }
  
  return(resQuery)
}

getAssociatedEnzymes <- function(nodeEName_, pId, org){
  sql <- paste0('SELECT n.nId, n.eName, na.childId, e.eName as enzymeName, m.link as link
          FROM nodeAlias as na INNER JOIN
          	enzime as e on e.eId = na.childId INNER JOIN
          	nodes as n on n.nId = na.nId INNER JOIN
			mapInfo as m on m.eId = na.childId AND pId = ',pId ,' AND orgId = \'',org ,'\'',
          'WHERE na.nId in (SELECT n.nId
          			FROM nodemetric nm INNER JOIN
          				nodes as n on n.nId = nm.nId  INNER JOIN
						path as p on p.pId = nm.pId
          			WHERE p.pId = "', pId, '" and n.ename = "', nodeEName_, '")
          	and na.type = "e";')
  
  resQuery <- dbGetQuery(dbCon,sql)
  return(resQuery)
}

getAssociatedReactions <- function(nodeEName_, pName_){
  sql <- paste0('SELECT na.nId, n.eName, na.childId, r.rName
          FROM nodeAlias as na INNER JOIN
            	reaction as r on r.rId = na.childId INNER JOIN
          	  nodes as n on n.nId = na.nId
          WHERE na.nId in (SELECT n.nId
            			FROM nodemetric nm INNER JOIN
            				nodes as n on n.nId = nm.nId  INNER JOIN
  						path as p on p.pId = nm.pId
            			WHERE p.pName = "', pName_, '" and n.ename = "', nodeEName_, '")
            	and na.type = "r";')
  
  resQuery <- dbGetQuery(dbCon,sql)
  return(resQuery)
}

getPIdFromName <- function(pathway){
  sql <- paste0('SELECT pId
          FROM path
          WHERE pName  = "', pathway,'";')
  
  pId<-as.integer(dbGetQuery(dbCon,sql)[1,1])
  if(is.na(pId)){
    stop(paste0('Pathway ',pathway,' not found'))
  }
  return(pId)
}

createNodeTable <- function(){
  sql<- 'SELECT DISTINCT nId 
        FROM nodeAlias
        ORDER BY nId'
  nodes <- dbGetQuery(dbCon,sql)[,1]
  
  counter <<- 1
  total <<- length(nodes)
  idx<-22
  for (idx in 1:total) {
    cat("Inserting node [",counter,"of",total,"]\n")
    counter<<-counter+1
    
    sql <- paste0(
            'SELECT eName
            FROM nodeAlias as na INNER JOIN
            	enzime as e on na.childId = e.eId
            WHERE type = "e" AND
            		na.nId = ', nodes[idx],
            ' ORDER BY eName')
    eNames <- dbGetQuery(dbCon,sql)[,1]
    eName <- eNames[1]
    if(length(eNames)>1){
      eName<-paste0(eName,"+")
    }
    eNameTmp <- eName
    counter2 <- 1
    repeat{
      repName<-checkNewName(name = eNameTmp,
                            table = 'nodes',
                            type = 'e')
      if(repName == 0){
        break
      }
      eNameTmp <- paste0(eName,"_",counter2)
      counter2 <- counter2 + 1
      # cat(eNameTmp,'\n')
    }
    eName <- eNameTmp

    sql <- paste0(
      'SELECT rName
            FROM nodeAlias as na INNER JOIN
            	reaction as r on na.childId = r.rId
            WHERE type = "r" AND
            		na.nId = ', nodes[idx],
      ' ORDER BY rName')
    rNames <- dbGetQuery(dbCon,sql)[,1]
    rName <- rNames[1]
    if(length(rNames)>1){
      rName<-paste0(rName,"+")
    }
    repeat{
      repName<-checkNewName(name = rName,
                            table = 'nodes',
                            type = 'r')
      if(repName == 0){
        break
      }
      rName <- paste0(rName,"*")
    }
    sql <- paste0('INSERT INTO nodes
                  VALUES (',
                  nodes[idx],',"',
                  eName,'","',
                  rName,'");')
    resQuery <- dbExecute(dbCon,sql)
  }
  
}


createDbConnection <- function(){
  if(!exists("dbCon")){
    closeDb <<- T
    dbDir<<-file.path(dirBase,"data","database")
    dbFile<<-file.path(dbDir,"dictionary.db")
    #conect and test dictionary
    dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
  }else if(!dbIsValid(dbCon)){
    closeDb <<- T
    dbDir<<-file.path(dirBase,"data","database")
    dbFile<<-file.path(dbDir,"dictionary.db")
    #conect and test dictionary
    dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)
  }    
  
  return(dbCon)
}

getPathId <- function(pathCode) {
  # DB attributes
  table <- "path"
  fields <- "pName"
  
  # Adjust the string escape
  values <- paste0("'", pathCode, "'")
  
  # Search in DB
  resQuery <- searchValue(table, fields, values)
  
  # Check if at least one pathway was returned
  if (is.null(resQuery) || length(resQuery) == 0) {
    stop("Pathway ", pathCode,' not found!')
  }
  
  return(resQuery)
}

getPathInfo <- function(pathwayinfo, 
                        orgName){
    table <- "path"
    fields <- "pName"
    
    pName<- sub(pattern = "path:",
                replacement = '', 
                x=pathwayinfo$name)
    pNameEC <- sub(pattern = orgName,
                 replacement = 'ec',
                 x = pName)
    pDesc<-pathwayinfo$title
    pImage<-pathwayinfo$image
    pLink <- pathwayinfo$link
    
    values<-paste0('"',pNameEC,'"')
    
    #pathway exists?
    nextId <- searchValue(table, fields, values)
    if(nextId == 0 ){ # Not Exists
      stop("Pathway ", pName,' not found!')
    }
    return(list(nextId, pName))
    
}


#reaction<-as.vector(reactionsRef[2,]) #debug
insertReactionOrg<-function(reaction){
  blFile <- file.path(dbDir,"blacklist")
  blackList<-read.csv(file = blFile,
                      header = T, 
                      stringsAsFactors =F)
  table <- "reaction"
  fields <- c("rName","rReversible")
  rName <- as.character(reaction["rName"])
  rType <- as.character(reaction["rType"])
  if(rName %in% blackList$Reaction){
    rType <- blackList$Reversible[blackList$Reaction == rName]
  }else{
    rType <- ifelse(rType == 'reversible', 1,0)
  }
  oldId<-reaction["rId"]
  pId <- reaction["pId"]
  org <- reaction["org"]
  
  values<-c(paste0('"',rName,'"'),
            paste0('"',rType,'"'))
  #reaction exists?
  nextId <- searchValue(table, fields, values)
  if(nextId ==0 ){ # Not Exists
    cat("Reaction ",rName," not found -",currentFile,"\n")
    cat(file = logFile,"Reaction ",rName," not found -",currentFile,"\n",append = T)
    return(0)
  }
  sql <- paste0('SELECT mainRId
                FROM reactionAssociation
                WHERE rId = ', nextId,';')
  nId <- dbGetQuery(dbCon,sql)[,1]

  table <- "nodebyorgs"
  fields <- c("nId","pId","org")
  values<-c(paste0('"',nId,'"'),
            paste0('"',pId,'"'),
            paste0('\'',org,'\''))
  
  exist <- searchValue(table, fields, values)
  if(exist == 0 ){ # Not Exists
    sql <- paste0('INSERT INTO nodebyorgs
                    VALUES (',
                  nId,',',
                  pId,',\'',
                  org,'\');')
    #cat(sql,' ',rName,' ',exist,'\n')
    resQuery <- dbExecute(dbCon,sql) 
    
  }

}

#mapL<-as.vector(entryMap[2,]) #debug
insertMap<-function(mapL){
  eIdOld<-mapL["eId"]
  pId <- mapL["pId"]
  orgId <- mapL["orgId"]
  link<- mapL["mLink"]
  x<- mapL["x"]
  y<- mapL["y"]
  # if(pId == 31){
  #   browser()
  # }
  
  sql<-paste0('SELECT eId
              FROM enzOnPath
              WHERE x = ',x,' AND y = ',y ,' AND pId = ', pId)  
  #cat(sql,x,y,'\n')
  eId <- dbGetQuery(dbCon,sql)
  if(nrow(eId) >0){
    if(nrow(eId)>1){
      eId<-eId[1,1]
    }
    table <- "mapInfo"
    fields <- c("eId","pId","orgId")
    values<-c(paste0('"',eId,'"'),
              paste0('"',pId,'"'),
              paste0('\'',orgId,'\''))
    
    exist <- searchValue(table, fields, values)
    if(exist == 0 ){ # Not Exists
      sql <- paste0('INSERT INTO mapInfo
                    VALUES (',
                    pId,',',
                    eId,',\'',
                    orgId,'\',\'',
                    link,'\');')
      resQuery <- dbExecute(dbCon,sql) 
    }
  }
  
}

getTotalOrgs <- function(pId_ = 0) {
  dbCon <- createDbConnection()
  
  if (!is.null(pId_) && pId_ != 0) {
    sql <- paste0('select count(DISTINCT org) as orgCount from nodebyorgs where pId = ', pId_)
  } else {
    sql <- 'select count(DISTINCT org) as orgCount from nodebyorgs'
  }
  
  orgCounts <- dbGetQuery(dbCon, sql)
  dbDisconnect(dbCon) 
  return(orgCounts[[1]])
}

getOrgCounts <- function(type = NA,
                         value = NA){
  dbCon <- createDbConnection()
  if(is.na(type) | is.na(value)){
    
    sql<-"select org, taxon, count(*) as count
    from nodebyorgs as no INNER JOIN
    (SELECT DISTINCT orgId, taxon as taxon
      FROM organism
      WHERE taxon = \'Eukaryotes\'
      UNION
      SELECT DISTINCT orgId, reino as taxon
      FROM organism
      WHERE reino = \'Bacteria\'
      UNION
      SELECT DISTINCT orgId, reino as taxon
      FROM organism
      WHERE reino = \'Archaea\') as o on o.orgId = no.org
    
    GROUP by org"

    #old select whit no taxon    
  # sql <- 'select org, count(*) as count
  #         from nodebyorgs
  #         GROUP by org'
  }else{
    if(!type %in% c("taxon","reino","filo","class")){
      cat("The parameter type must be taxon, reino, filo orclass \n\n")
      return(0)
    }else{
      value <- paste0('\'',value,'\'')
      sql <- paste0('select org, count(*) as count
          from nodebyorgs
          WHERE org in (
                  SELECT orgId
                  FROM organism
                  WHERE ',type, ' in ( ',value,') )
          GROUP by org')
    }
  }
  
  orgCounts <- dbGetQuery(dbCon,sql)
  
  dbDisconnect(dbCon) 
  
  return(orgCounts)
}

countNodeFrequency <- function(nId_, pId_) {
  dbCon <- createDbConnection()
  sql <- paste0('select count(DISTINCT org) as orgCount from nodebyorgs where nId = ', nId_, ' and pId = ', pId_)
  orgCounts <- dbGetQuery(dbCon, sql)
  dbDisconnect(dbCon) 
  return(orgCounts[[1]])
}

getAPCountByOrg <- function(orgs){
  dbCon <- createDbConnection()

  orgCount <- length(orgs)

  condition <-paste0(orgs, collapse = '\',\'')
  condition <- paste0('\'',condition,'\'')
  
  sql <- paste0(
    'SELECT p.pName, no.pId, n.eName, no.nId, n.rName, nm.isAP, count(*) as occurrences
      FROM nodebyorgs as no INNER JOIN
      nodemetric as nm on nm.pId = no.pId AND
      nm.nId = no.nId INNER JOIN
      nodes as n on n.nId = nm.nId INNER JOIN
      path as p on p.pId = no.pId
      WHERE org in (',condition,')
      GROUP BY no.nId, no.pId
      ORDER BY no.pId, no.nId')
  APCounts <- dbGetQuery(dbCon,sql)
  dbDisconnect(dbCon)
  # Remove proteins with zero occurrence
  APCounts$percentage <- APCounts$occurrences/orgCount
  APCounts$total<- orgCount
  
  #**************************************************************************##
  # Apply a normalization:                                              #
  # 100% of occurrence in a pathway can be compared with 50% of other pathway #
  #**************************************************************************##
  
  # Get the unique pathways
  uniquePathways <- unique(APCounts$pName)
  
  # Calculates the normalized frequency
  for (item in uniquePathways) {
    # Max frequency in a pathway
    pathwayMaxFrequency <- max(APCounts$percentage[APCounts$pName==item])
    
    # Min frequency in a pathway
    pathwayMinFrequency <- min(APCounts$percentage[APCounts$pName==item])
    
    # Normalized frequency for each protein
    APCounts$nPercent[APCounts$pName==item] <-
      (APCounts$percentage[APCounts$pName==item]-pathwayMinFrequency)/
      (pathwayMaxFrequency-pathwayMinFrequency)
  }
  
  # Fix for NAN cases (when min and max frequency have the same values)
  APCounts$nPercent[is.nan(APCounts$nPercent)] <- APCounts$percentage[is.nan(APCounts$nPercent)]/100
  
  return(APCounts)
}

stratifyAPs <- function(apCounts,
                        interval = 0.10,
                        normalized = T){
  if(normalized){
    apCounts$percent<-apCounts$nPercent
  }else{
    apCounts$percent<-apCounts$percentage
  }
  
  # Filter dataSet from proteins with ZERO frequency
  apCounts <- apCounts[!apCounts$occurrences==0,]
  
  # Order the dataSet
  apCounts <- apCounts[order(apCounts$percent, decreasing = T),]
  
  
  #*****************************************##
  # Generate the stratified     distribution #
  #*****************************************##
  
  # Count the bottlenecks and non-bottlenecks
  countsBase <- c(APs=nrow(apCounts[apCounts$isAP ==1,]), 
                  nAPs=nrow(apCounts[apCounts$isAP !=1,]))
  
  proportion<- countsBase[1]/(countsBase[1]+countsBase[2])
  
  ranges<-seq(0,1,interval)
  
  # Create a dataFrame for the result
  distribution <- list()
  # Loop over all dataSet
  idx=1
  for (idx in 1:(length(ranges)-1)) {
    # Set the cumulative range [initVal:range]
    initVal <- ranges[idx]
    
    # Retrieve the cumulative proteins
    if(idx == length(ranges)-1){
      top <- apCounts[apCounts$percent>=ranges[idx]&
                        apCounts$percent<=ranges[idx+1], ]
      range<-ranges[idx+1]
    }else{
      top <- apCounts[apCounts$percent>=ranges[idx]&
                        apCounts$percent<ranges[idx+1], ]
      range<-ranges[idx+1]-0.001
    }
    # Number of draws
    drawn <- nrow(top)
    
    # The number of articulation points in the accumulated group
    AP <- nrow(top[top$isAP == 1,])
    nAP <- nrow(top[top$isAP == 0,])
    if(AP+nAP != drawn ){
      stop('Number of AP + nAP doesn\'t match with totoal')
    }

    distribution[[idx]]<- data.frame(ini=initVal,
                              range=range,
                              AP=AP,
                              nAP=nAP,
                              stringsAsFactors = F)
  }
  
  distribution <- do.call(rbind,distribution)
  
  return(list(distribution,proportion))
}


#FIM ----
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

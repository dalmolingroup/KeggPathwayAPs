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
  
  command<-paste("cat ",dbTemplate,
                 " | sqlite3 ", dbFile)
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

  sql <- paste0('select r.rId,
                        case when r.rReversible = 0 then "R" ELSE "I" END ||
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

prepareReacAssos <- function(){
  #prepare the table reactionAssociation to receive data
  sql <- "DELETE FROM reactionAssociation"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM edges"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "DELETE FROM fakeNode"
  resQuery <- dbExecute(dbCon,sql)
  sql <- "update fakeEdge set nextId = 100000"
  resQuery <- dbExecute(dbCon,sql)
  
  
  sql <- "INSERT INTO reactionAssociation
          SELECT DISTINCT rId, 0
          FROM reaction;"
  resQuery <- dbExecute(dbCon,sql)
}

#reactList2<-reactList[1,] #debug
insertReacList <- function(reactList2){
  #make the correlation between reactions with same 
  # subtrate and product
  reactList2<-reactList2[-2]#$dup_count<-NULL
  reactList2<-as.vector((reactList2[!is.na(reactList2)]))
  #cat(reactList2,'\n')
  idx<-3
  #if(length(reactList2) > 2){
    for(idx in 2:length(reactList2)){
      sql <- paste0("select * 
                    from reactionAssociation
                    where rId = ", reactList2[idx]," and 
                    mainRId != 0;")
      resQuery <- dbGetQuery(dbCon,sql)
      if(nrow(resQuery) > 0){
        cat("Reaction", reactList2[idx], 'already processed. Overwriting!')
      }
      sql<-paste0('UPDATE reactionAssociation
          SET mainRId = ',reactList2[2],'
          WHERE rId = ',reactList2[idx],';')
      resQuery <- dbExecute(dbCon,sql)
    }
  #}
}

# reactList2<-reactList[9,] #debug
# insertEdges(reactList2)
insertEdges <- function(reactList2){
  cat("Inserting edges [",counter,"of",total,"]\n")
  counter<<-counter+1
  reactList2<-reactList2[-2]#$dup_count<-NULL
  reactList2<-as.vector((reactList2[!is.na(reactList2)]))
  #cat(reactList2,'\n')
  idx<-3
  nId<-as.numeric(trim(reactList2[2]))
  # sql <- paste0('select rName
  #               from reaction
  #               where rId = ',nId)
  # 
  # rName<- substring(dbGetQuery(dbCon,sql)[1,1],4)

  sql <- paste0('SELECT eName
                FROM enzime as e INNER JOIN
                    enzReac as er on er.eId = e.eId
                WHERE er.rId =', nId,'
                order by eName')

  eName<- dbGetQuery(dbCon,sql)
  
  nName <- eName[1,1]
  # fName <- paste0('f',
  #                 substring(
  #                   gsub(pattern = '[.]',
  #                        replacement = '',
  #                        nName),4) )
  if(nrow(eName)>1){
    nName<-paste0(nName,"+")
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
                                      subs=as.numeric(subs[idx2]),
                                      prod=fakeIdS,
                                      type = "F",
                                      reversible = isRevers,
                                      stringsAsFactors = F)
      idxEdge <- idxEdge +1
      if(isRevers == 1){
        edgeList[[idxEdge]]<-data.frame(nId = nId,
                                        nName = fakeNameS,
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
                                      subs=fakeIdP,
                                      prod=as.numeric(prod[idx2]),
                                      type = "F",
                                      reversible = isRevers,
                                      stringsAsFactors = F)

      idxEdge <- idxEdge +1
      if(isRevers == 1){
        edgeList[[idxEdge]]<-data.frame(nId = nId,
                                        nName = fakeNameP,
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
                                    subs=fakeIdS,
                                    prod=fakeIdP,
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = fakeNameP,
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
                                    subs=fakeIdS,
                                    prod= prod[1],
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
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
                                    subs = subs[1],
                                    prod=fakeIdP,
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
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
                                    subs=subs[1],
                                    prod=prod[1],
                                    type = "R",
                                    reversible = isRevers,
                                    stringsAsFactors = F)
    
    idxEdge <- idxEdge +1
    if(isRevers == 1){
      edgeList[[idxEdge]]<-data.frame(nId = nId,
                                      nName = nName,
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
                  x["nName"], '",',
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
                              "cpd:C11481","cpd:C14818")
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


showGraph<-function(ecs = NA, 
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
    sql<-paste0('select eName 
                from enzOnPath as ep inner join
                path as p on p.pId = ep.pId inner Join
                enzime as e on e.eId = ep.eId
                where pName = "',pathway,'"') 
    ecs<- dbGetQuery(dbCon,sql)
    ecs <- as.vector(ecs[,1])
  }
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
  g1 <- graph_from_data_frame(edges, 
                                   directed=TRUE, 
                                   vertices=NULL)
  
  edgeNames<-E(g1)$eName
  edge_attr(g1)
    #print(g1, e=TRUE, v=TRUE)
  edge_attr(g1) <- list(color = rep("black", gsize(g1)),
                       curved = rep(F, gsize(g1)))
  edge_attr(g1, "label") <- edgeNames 
  #tkplot(g1)

  attrs<-data.frame(color='cyan',
                    name = V(g1)$name,
                    stringsAsFactors = F)
  attrs$color[grep('fec:',attrs$name)]<-"red"
  
  vertex_attr(g1) <- list(name= attrs$name,
                          color = attrs$color)

  g3<-cleanedLineGraph(g1, removeFake = removeFake)
  
  
  g4<-graph_from_data_frame(g3, directed = T)
  
  attrs<-data.frame(color='yellow',
                    name = V(g4)$name,
                    stringsAsFactors = F)
  attrs$color[grep('fec:',attrs$name)]<-"red"
  vertex_attr(g4) <- list(name= attrs$name,
                          color = attrs$color)
  
  tk1<-tkplot(g1,canvas.width = 1200, canvas.height = 650)
  
  tk4<-tkplot(g4,canvas.width = 1200, canvas.height = 650)
  tk_center(tk4)
  
  dbDisconnect(dbCon)  
  return(g4)

  #plot(edges)
}

cleanedLineGraph <- function(g1, removeFake = F){
  
  
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

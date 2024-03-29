#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#*************************************#
# Functions to handle KEGG kgml files #
#*************************************#

# kgmlFunctions.R #

# ---- IMPORT SECTION ----

#' This is the set of functions to handle the KEGG kgml files
#'
#' @author
#' Igor Brandão & Clovis F Reis

# Import the necessary libraries
library("XML")

#*******************************************************************************************#

# ---- KGML FUNCTIONS ----

.xmlChildrenWarningFree <- function(xmlNode)
{
  # Verify if the XML node has a child node
  if (is.null(xmlNode$children))
    return(NULL)
  return(XML::xmlChildren(xmlNode))
}

parsePathwayInfo <- function(root)
{
  # Declare the pathwayInfo dataframe
  pathwayInfo <- data.frame(name = NA, org = NA, number = NA, title = NA, image = NA,
                            link = NA, stringsAsFactors = FALSE)

  # Access the entry attributes
  attrs <- xml_attrs(root)

  # Retrieve the 1-dimensional attributes
  name <- attrs[["name"]]
  org <- attrs[["org"]]
  number <- attrs[["number"]]
  title <- getNamedElement(attrs, "title")
  image <- getNamedElement(attrs, "image")
  link <- getNamedElement(attrs, "link")

  # Set the pathwayInfo attributes
  pathwayInfo$name <- name
  pathwayInfo$org <- org
  pathwayInfo$number <- number
  pathwayInfo$title <- title
  pathwayInfo$image <- image
  pathwayInfo$link <- link

  # Return the pathwayInfo dataFrame
  return(pathwayInfo)
}

parseGraphics <- function(graphics)
{
  # Declare the graphic dataframe
  g <- data.frame(x = NA, y = NA, graphicalType = NA, width = NA, height = NA, fgcolor = NA,
                  bgcolor = NA, stringsAsFactors = FALSE)

  # Check if the graphics contains data
  if (is.null(graphics))
    return(g)

  # Access the entry attributes
  attrs <- xmlAttrs(graphics)

  # Set the graphical attributes
  g$x <- as.integer(getNamedElement(attrs, "x"))
  g$y <- as.integer(getNamedElement(attrs, "y"))
  g$graphicalType <- getNamedElement(attrs, "type")
  g$width <-as.integer(getNamedElement(attrs, "width"))
  g$height <- as.integer(getNamedElement(attrs, "height"))
  g$fgcolor <- getNamedElement(attrs, "fgcolor")
  g$bgcolor <- getNamedElement(attrs, "bgcolor")

  # Return the graphics dataFrame
  return(g)
}

parseEntry <- function(entry)
{
  # Declare the node dataframe
  # newNode <- data.frame(entryID = NA, name = NA, type = NA, link = NA, component = NA,
  #                           reaction = NA, map = NA, x = NA, y = NA, graphicalType = NA,
  #                           width = NA, height = NA, fgcolor = NA, bgcolor = NA, stringsAsFactors = FALSE)


  # newEntryRef strucutre : eId, eName, eType, eReaction, multiReact, x, y coordinates
  # newEntryMap strucutre : eId, mLink, mName, fgcolor, bgcolor,
  #                         type, width, height
  # newEntryOther strucutre : eId, oType, oData 
  
  newEntryRef <- list()
  newEntryMap <- list()
  dupReaction <- list()
  dupEntry <- list()
  
  
  
  # Access the entry attributes
  attrs <- xml_attrs(entry)
  
  
  
  # Retrieve the 1-dimensional attributes
  eId <- attrs[["id"]]
  eType <- attrs[["type"]]
  #cat(eId,"\n")
  if('reaction' %in% names(attrs)){
    eReaction <- unlist(strsplit(attrs[["reaction"]],split = " "))
    #if are more than one reaction name per line
    #colapse to the first one and anotate
    if(length(eReaction) == 1){
      dupReaction[[1]]<-data.frame(eId=eId, reactions=NA,
                                stringsAsFactors = F)
    }else{
      dupReaction[[1]]<-data.frame(eId=eId, reactions=attrs[["reaction"]],
                                stringsAsFactors = F)
      eReaction = eReaction[[1]]
    }
    
  }else{
    eReaction <- NA
  }
  eNames <- unlist(strsplit(attrs[["name"]],split = " "))

  # Retrieve map attributes
  mLink <- getNamedElement(attrs, "link")
  map <- getNamedElement(attrs, "map")
  graphics <- xml_children(entry)
  #eId, mLink, mName, fgcolor, bgcolor,
  #                         type, x, y, width, height
  # Retrieve the N-dimensional attributes
  g <- unlist(xml_attrs(graphics))

  # Verify the node type
  if (eType != "group") {
    # Set the map attributes
    component <- NA
  }else if (type == "group") {
    children <- xml_children(entry)
    children <- children[names(children) == "component"]

    if (length(children) == 0) {
      component <- as.character(NA)
    }
    else {
      component <- sapply(children, function(x) {
        if (xmlName(x) == "component") {
          return(xmlAttrs(x)["id"])
        }
        else {
          return(as.character(NA))
        }
      })
    }

    component <- unname(unlist(component))
    component <- paste(component, collapse = " / ")
    
  }

  #if are more than one name per line
  if(length(eNames) == 1){
    newEntryRef[[1]]<-data.frame(eId=eId,eName=eNames,
                                 eType=eType, eReaction = eReaction, 
                                 x = g[["x"]], y = g[["y"]],
                                 stringsAsFactors = F)
    dupEntry[[1]]<-data.frame(eId=eId, eNames=NA,
                              stringsAsFactors = F)
  }else{
    dupEntry[[1]]<-data.frame(eId=eId, eNames=attrs[["name"]],
                              stringsAsFactors = F)
    for (idx in 1:length(eNames)) {
      newEntryRef[[idx]]<-data.frame(eId=eId,eName=eNames[[idx]],
                                     eType=eType, eReaction = eReaction,
                                     x = g[["x"]], y = g[["y"]],
                                     stringsAsFactors = F)
    }
  }

    newEntryMap[[1]]<-data.frame(eId=eId, map=map, mLink=mLink,
                               graphicalType = g[["type"]],
                               width= g[["width"]], height = g[["height"]],
                               fgcolor = g[["fgcolor"]], bgcolor = g[["bgcolor"]],
                               stringsAsFactors = F)

  newEntryRef<-do.call(rbind, newEntryRef)
  newEntryMap<-do.call(rbind, newEntryMap)
  dupEntry<-do.call(rbind, dupEntry)
  dupReaction<-do.call(rbind, dupReaction)
  
  # Return the node as a dataframe
  return(list(newEntryRef,
              newEntryMap,
              dupEntry,
              dupReaction))
}

parseSubType <- function(subtype)
{
  # Declare the subtype dataframe
  subtypeDf <- data.frame(name = NA, value = NA, stringsAsFactors = FALSE)

  # Access the subtype attributes
  attrs <- xml_attrs(subtype)

  # Retrieve the 1-dimensional attributes
  subtypeDf$name <- attrs[["name"]]
  subtypeDf$value <- attrs[["value"]]

  # Return the subtype dataFrame
  return(subtypeDf)
}

parseRelation <- function(relation)
{
  # Declare the newRelation dataframe
  newRelation <- data.frame(entry1 = NA, entry2 = NA, type = NA, 
                            subtype = NA, value = NA,
                            stringsAsFactors = FALSE)

  # Access the relation attributes
  attrs <- xml_attrs(relation)

  # Retrieve the 1-dimensional attributes
  newRelation$entry1 <- attrs[["entry1"]]
  newRelation$entry2 <- attrs[["entry2"]]
  newRelation$type <- attrs[["type"]]

  # Retrieve the N-dimensional attributes
  subtypeNodes <- unlist(xml_attrs(xml_children(relation)))
  
  # subtypes <- sapply(subtypeNodes, parseSubType)
  # subtypes <- paste(subtypes, collapse = " / ")

  # Apply the subtypes into the dataFrame
  newRelation$subtype <- subtypeNodes[["name"]]
  newRelation$value <- subtypeNodes[["value"]]
  
  # Return the edge dataFrame
  return(newRelation)
}

parseReaction <- function(reaction)
{
  # Declare the newReaction dataframe
  # newReaction <- data.frame(name = NA, type = NA, children = NA, childrenNames = NA, substrateIndices = NA,
  #                           productIndices = NA, substrateName = NA, productName = NA, stringsAsFactors = FALSE)
  
  # newReactionRef strucutre :rId, rName, rType
  # newReactionDef strucutre :rId, cpdType, cpdId, cpdName
  # dupReactions strucutre   :rId, rNames
  
  newReactionRef <- list()
  newReactionDef <- list()
  dupReactions <- list()
  
  # Access the reaction attributes
  attrs <- xml_attrs(reaction)

  # Retrieve the 1-dimensional attributes
  rId <- attrs[["id"]]
  rType <- attrs[["type"]]
  rNames <- unlist(strsplit(attrs[["name"]],split = " "))
  #if are more than one reaction name per line
  if(length(rNames) == 1){
      newReactionRef[[1]]<-data.frame(rId=rId,rName=rNames,rType=rType, 
                                      stringsAsFactors = F)
      dupReactions[[1]]<-data.frame(rId=rId, rNames=NA,
                                    stringsAsFactors = F)
  }else{
      dupReactions[[1]]<-data.frame(rId=rId, rNames=attrs[["name"]],
                                    stringsAsFactors = F)
      for (idx in 1:length(rNames)) {
        newReactionRef[[idx]]<-data.frame(rId=rId,rName=rNames[[idx]],rType=rType, 
                                          stringsAsFactors = F)
      }
    }
  
  dupReactions<-do.call(rbind, dupReactions)
  newReactionRef<-do.call(rbind, newReactionRef)
  
  #colnames(newReactionRef)<-c("rId", "rName", "rType")
  
  reactionChildren <- xml_children(reaction)

  
  #get substrates and products
  if(length(reactionChildren) == 0){
    newReactionDef[[1]]<- data.frame(rId = rId, cpdType= NA, cpdId=NA, cpdName=NA, 
                                     stringsAsFactors = F)
  }else{
    idx<-1
    for(idx in 1:length(reactionChildren)){
      cpdType <- xml_name(reactionChildren[[idx]])
      attrs <- xml_attrs(reactionChildren[[idx]])
      cpdId<-attrs[["id"]]
      cpdName<-attrs[["name"]]
      newReactionDef[[idx]]<- data.frame(rId = rId, cpdType=cpdType, cpdId=cpdId, cpdName=cpdName, 
                                stringsAsFactors = F)
    }
    newReactionDef<-do.call(rbind, newReactionDef)
    #colnames(newReactionDef)<-c("rId", "cpdType", "cpdId", "cpdName")
  }
  
  dupReactions <- na.exclude(dupReactions)
  # Return the reaction dataFrame
  return(list(newReactionRef,newReactionDef, dupReactions))
  
  # 
  # newReaction$children <- paste(as.character(reactionChildren), collapse = " / ")
  # 
  # childrenNames <- paste(as.character(names(xmlChildren(reaction))), collapse = " / ")
  # newReaction$childrenNames <- childrenNames
  # 
  # substrateIndices <- grep("substrate", childrenNames)
  # productIndices <- grep("product", childrenNames)
  # 
  # newReaction$substrateIndices <- paste(as.character(grep("substrate", childrenNames)), collapse = " / ")
  # newReaction$productIndices <- paste(as.character(grep("product", childrenNames)), collapse = " / ")
  # 
  # # Retrieve the N-dimensional attributes
  # substrateAltName <- paste(as.character(vector("character", length(substrateIndices))), collapse = " / ")
  # productAltName <- paste(as.character(vector("character", length(productIndices))), collapse = " / ")
  # 
  # # Adjust the names
  # substrateAltName <- paste(substrateAltName, collapse = " / ")
  # productAltName <- paste(productAltName, collapse = " / ")
  # 
  # newReaction$substrateName <- substrateAltName
  # newReaction$productName <- productAltName
  # 
  # for (i in seq(along = substrateIndices)) {
  #   ind <- substrateIndices[i]
  #   substrate <- children[[ind]]
  #   substrateName[i] <- xmlAttrs(substrate)[["name"]]
  #   substrateAltName[i] <- as.character(NA)
  #   substrateChildren <- .xmlChildrenWarningFree(substrate)
  #   if (!is.null(substrateChildren)) {
  #     substrateAlt <- substrateChildren$alt
  #     substrateAltName[i] <- xmlAttrs(substrateAlt)[["name"]]
  #   }
  # }
  # for (i in seq(along = productIndices)) {
  #   ind <- productIndices[i]
  #   product <- children[[ind]]
  #   productName[i] <- xmlAttrs(product)[["name"]]
  #   productChildren <- .xmlChildrenWarningFree(product)
  #   productAltName[i] <- as.character(NA)
  #   if (!is.null(productChildren)) {
  #     productAlt <- productChildren$alt
  #     productAltName[i] <- xmlAttrs(productAlt)[["name"]]
  #   }
  # }
  # 
  # # Adjust the names
  # substrateAltName <- paste(substrateAltName, collapse = " / ")
  # productAltName <- paste(productAltName, collapse = " / ")
  # 
  # newReaction$substrateAltName <- substrateAltName
  # newReaction$productAltName <- productAltName

}

#*******************************************************************************************#

# ---- MAIN FLOW FUNCTION ----

#' Get the KEGG kgml file and parse its information
#'
#' Given a KEGG kgml file, this function parse this file and returns a data.frame contaning
#' all relevante information related to this pathway
#'
#' @param kgml_ Name of KGML file.
#'
#' @return This function returns a list of data.frames containing all relevante information
#' related to a pathway
#'
#' @examples
#' \dontrun{
#' lst <- kgmlToDataframe('ko00010.xml')
#' }
#'
#' @importFrom 'XML'
#'
#' @author
#' Igor Brandão

KGML2Dataframe <- function(kgml_,
                           dataType,
                           dbCon) {
  # Verify and process the KGML file

    # tryCatch(doc <- xmlTreeParse(kgml_, getDTD = FALSE, error = xmlErrorCumulator(immediate = FALSE)),
    #        error = function(e) {
    #          fileSize <- file.info(kgml_)$size[1]
    #          bytes <- sprintf("%d byte%s", fileSize, ifelse(fileSize >
    #                                                           1, "s", ""))
    #          msg <- paste("The file", kgml_, "seems not to be a valid KGML file\n")
    #          if (fileSize < 100L)
    #            msg <- paste(msg, "[WARNING] File size (", bytes,
    #                         ") is unsually small; please check.\n", sep = "")
    #          msg <- paste(msg, "\nDetailed error messages from",
    #                       "XML::xmlTreeParse:\n", sep = "")
    #          cat(msg)
    #          stop(e)
    #        })
  
  error <<- 0
  tryCatch(doc <- read_xml(kgml_, getDTD = FALSE, error = xmlErrorCumulator(immediate = FALSE)),
           error = function(e) {
             fileSize <- file.info(kgml_)$size[1]
             bytes <- sprintf("%d byte%s", fileSize, ifelse(fileSize >
                                                              1, "s", ""))
             msg <- paste("The file", kgml_, "seems not to be a valid KGML file\n")
             if (fileSize < 100L)
               msg <- paste(msg, "[WARNING] File size (", bytes,
                            ") is unsually small; please check.\n", sep = "")
             msg <- paste(msg, "\nDetailed error messages from",
                          "XML::xmlTreeParse:\n", sep = "")
             cat(msg)
             cat(file = logFile, append = T,
                 "\tError processing", basename(kgml_), "file.\n",msg,'\n')
             error<<-1
             return(NULL)
           })
  if(error != 0){
    return(0)
  }  
  # Retrieve the XML root
  root <- xml_root(doc)
  #r1<- read_xml(kgml_)
  #childnames <- sapply(xml_children(root), xml_name)
  # # r[[1]]
  # # r[[2]]
  # # 
  # # l<-seq(700,800,1)
  # # length(xmlChildren(r))
  #    r<-removeChildren(node = r,kids = s)
  # # 
  # # saveXML(r, file = file.path(folder,"01teste.xml"))
  # # r[[1]]
  # # r[[2]]
  # 
  # # Retrieve the XML root children
  # childnames <- sapply(xmlChildren(r), xmlName)
  # 
  # r[names(r)=="entry"]
  # 
  # s<-xml_find_first(r1,"//entry[@id='33']")
  # 
  # xml_attr(s,"id")<-"33"
  # 
  # xml_remove(s)
  # write_xml(r1,file = "/home/clovis/Projetos/GrupoDalmolin/Igor/gitAPs/data/kgml/ec/01teste.xml")
  # 
  # getNodeSet(r1, "//entry[@id='48']")

  # Define which children are entries
  isEntry<-xml_find_all(root,"//entry")
  #isEntry <- childnames == "entry"
  
  # Define which children are relations
  isRelation<-xml_find_all(root,"//relation")
  # isRelation <- childnames == "relation"

  # Define which children are reactions
  isReaction<-xml_find_all(root,"//reaction")
  # isReaction <- childnames == "reaction"
  
  # Retrieve the pathway info as dataFrame
  pathwayinfo <- parsePathwayInfo(root)
  map <- sub(x = pathwayinfo$name, pattern = "path:",replacement = '')
  
  entry<-xml_find_first(root,"//entry[@name='ec:2.7.1.1']") #debug
  #entry<-isEntry[[1]] #debug
  # Retrieve the pathway entries as dataFrame
  tryCatch(dataList <- lapply(isEntry, parseEntry),
           error = function(e) {
             cat("\tError processing", basename(kgml_), "file.\n")
             cat(file = logFile, append = T,
                 "\tError processing", basename(kgml_), "file.\n")
             error<<-1
             return(NULL)
           })
  if(error != 0){
    return(0)
  }
             
           
  #rearange in dataframes
  entryRef <- do.call(rbind, lapply(dataList, `[[`, 1))
  entryMap <- do.call(rbind, lapply(dataList, `[[`, 2))
  dupEntry <- do.call(rbind, lapply(dataList, `[[`, 3))
  dupEntry<-na.exclude(dupEntry)
  eDupReaction <- do.call(rbind, lapply(dataList, `[[`, 4))
  eDupReaction<-na.exclude(eDupReaction)
  rm(dataList)

  relation<-xml_find_first(root,"//relation") #debug
  # Retrieve the pathway edges as dataFrame
  tryCatch(dataList <- lapply(isRelation, parseRelation),
                error = function(e) {
                  cat("\tError processing", basename(kgml_), "file.\n")
                  cat(file = logFile, append = T,
                      "\tError processing", basename(kgml_), "file.\n")
                  error<<-1
                  return(NULL)
                })
  if(error != 0){
    return(0)
  }
  
  relationRef <- do.call(rbind, dataList)
  rm(dataList)

  # Retrieve the pathway reactions as dataFrame
  
  tryCatch(dataList <- lapply(isReaction, parseReaction),
                error = function(e) {
                  cat("\tError processing", basename(kgml_), "file.\n")
                  cat(file = logFile, append = T,
                      "\tError processing", basename(kgml_), "file.\n")
                  error<<-0
                  error<<-1
                  return(NULL)
                })
  if(error != 0){
    return(0)
  }
  
  # reactions <- do.call(rbind, dataList)
  reactionsRef <- do.call(rbind, lapply(dataList, `[[`, 1))
  reactionsDef <- do.call(rbind, lapply(dataList, `[[`, 2))
  rDupReaction <- do.call(rbind, lapply(dataList, `[[`, 3))
  rDupReaction<-na.exclude(rDupReaction)
  rm(dataList)
  
  #separate enzimes from maps and compounds
  if(dataType == 'ec'){
    enzimes<-entryRef[entryRef$eType == 'enzyme',]
  }else{
    enzimes<-entryRef[entryRef$eType == 'gene',]
  }
  compounds<-entryRef[entryRef$eType == 'compound',]
  compounds$cDesc<-''
  # #remove duplicated pair enzime + reaction
  # enzimes<-enzimes[!duplicated(enzimes[,c("eName","eReaction")]),]
  
  # To maintain always the lower name
  enzimes<-enzimes[order(enzimes$eName),]
  
  
  
  #colapse alternative enzimes for the same reaction
  nodes<-enzimes[!duplicated(enzimes[,c("eReaction")]),]
  nodesDuplic<-enzimes[duplicated(enzimes[,c("eReaction")]),]
  nodesDuplic<- merge(nodesDuplic, nodes[,c("eId","eName",
                                            "eReaction",
                                            "x","y")],
                      by="eReaction")
  
  #nodes<-current_kgml$nodes
  map <- pathwayinfo$number


  i=2 #debug
  # dbDisconnect(dbCon) #debug
  # dbCon <- dbConnect(RSQLite::SQLite(), dbFile)#debug
  
  #something is wrong with data?
  if(isTRUE(is.null(nrow(compounds))) | 
     isTRUE(nrow(compounds) == 0) |
     isTRUE(is.null(nrow(reactionsRef))) | 
     isTRUE(nrow(reactionsRef) == 0)|
     isTRUE(is.null(nrow(enzimes))) | 
     isTRUE(nrow(enzimes) == 0) |
     isTRUE(is.null(nrow(relationRef))) | 
     isTRUE(nrow(relationRef) == 0)){
    cat("\tError processing", basename(kgml_), "file.\n")
    cat(file = logFile, append = T,
        "\tError processing", basename(kgml_), "file.\n")
    return(0)
  }
    
    
  if(dataType == 'ec'){
    # EC ----
    #Insert pathway information into database
    pId <- do.call(rbind, insertPathInfo(pathwayinfo))[1]
                   
    enzimes$pId<-pId
    compounds$pId<-pId
    reactionsRef$pId<-pId
    reactionsDef$pId<-pId
    
    compoundsNId <- do.call(rbind, 
                   apply(X = compounds,
                         MARGIN = 1,
                         insertCompound))

    ltEnzReac<-list(enzimes,reactionsRef, 
                    reactionsDef, compoundsNId,
                    pId) #debug
    ltEnzReac <- insertEnzReac(list(enzimes,reactionsRef, 
                       reactionsDef, compoundsNId, 
                       pId))
    
    enzimes2<-ltEnzReac[[1]]
    reactionsRef2 <- ltEnzReac[[2]]
    reactionsDef2 <- ltEnzReac[[3]]
    
    #remove all maplinks
    relationRef2 <- relationRef[relationRef$type == "ECrel",]
    #get entry1 newId
    relationRef2<- merge(relationRef2,
                         enzimes2[,c("newId","oldId")],
                         by.x = "entry1",
                         by.y = "oldId",
                         all.x = T)
    colnames(relationRef2)[c(1,6)] <- c("eOldId1","eNewId1")
    #get entry2 newId
    relationRef2<- merge(relationRef2,
                         enzimes2[,c("newId","oldId")],
                         by.x = "entry2",
                         by.y = "oldId",
                         all.x = T)
    colnames(relationRef2)[c(1,7)] <- c("eOldId2","eNewId2")
    #get compound newId
    relationRef2 <- merge(relationRef2,
                          compoundsNId[,c("newId","oldId")],
                          by.x = "value",
                          by.y = "oldId",
                          all.x = T)
    colnames(relationRef2)[c(1,8)] <-c("cOldId","cNewId")
    
    relationRef2 <- na.exclude(relationRef2)
    
    relationRef3 <- do.call(rbind, 
                            apply(X = relationRef2,
                                  MARGIN = 1,
                                  insertRelation))
    
    #prepare entryMap to insertion
    #map<-entryMap
    
    #remove ORTHOLOGY links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]K',
                                             x = entryMap$mLink)],]
    #remove compound links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]C',
                                             x = entryMap$mLink)],]
    
    #remove map links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]ec',
                                             x = entryMap$mLink)],]
    #remove map links
    entryMap<-entryMap[entryMap$graphicalType != 'roundrectangle',]
    
    
    entryMap$map<-NULL
    entryMap$graphicalType<-NULL
    entryMap$width<-NULL
    entryMap$height<-NULL
    entryMap$fgcolor<-NULL
    entryMap$bgcolor<-NULL
    entryMap$pId<-pId
    entryMap$orgId<-dataType
    tmp<-unique(entryRef[,c('eId','x','y')])
    entryMap<- merge(entryMap,tmp, by ='eId')
    
    apply(X = entryMap,
          MARGIN = 1,
          insertMap)
    
    
  }else{
    # other orgs ----
    #process organism information
    pId <- do.call(rbind, 
                   getPathInfo(pathwayinfo = pathwayinfo,
                               orgName = dataType))[1]
    
    enzimes$pId<-pId
    enzimes$org <- dataType
    compounds$pId<-pId
    reactionsRef$pId<-pId
    reactionsRef$org <- dataType
    reactionsDef$pId<-pId
    
    apply(X = reactionsRef,
          MARGIN = 1,
          insertReactionOrg)
    
    #prepare entryMap to insertion
    #map<-entryMap
    
    #remove ORTHOLOGY links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]K',
                                             x = entryMap$mLink)],]
    #remove compound links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]C',
                                             x = entryMap$mLink)],]
    
    #remove map links
    entryMap<-entryMap[!entryMap$mLink %in%
                         entryMap$mLink[grep(pattern = 'www_bget[?]ec',
                                             x = entryMap$mLink)],]
    #remove map links
    entryMap<-entryMap[entryMap$graphicalType != 'roundrectangle',]
    
       
    entryMap$map<-NULL
    entryMap$graphicalType<-NULL
    entryMap$width<-NULL
    entryMap$height<-NULL
    entryMap$fgcolor<-NULL
    entryMap$bgcolor<-NULL
    entryMap$pId<-pId
    entryMap$orgId<-dataType
    tmp<-unique(entryRef[,c('eId','x','y')])
    entryMap<- merge(entryMap,tmp, by ='eId')
    
    apply(X = entryMap,
          MARGIN = 1,
          insertMap)
    
    
    }

  return(1)
  
}

#' Get the KEGG kgml file and convert it into graph
#'
#' Given a KEGG kgml file, this function parse this file and returns the graph structure
#'
#' @param kgml_ Name of KGML file.
#' @param replaceOrg_ Flag to determine with wheter or not the organism name will be changed.
#' @param orgToReplace_ The organism identification.
#'
#' @return This function returns a list of data.frames containing all relevante information
#' related to a pathway
#'
#' @examples
#' \dontrun{
#' graph <- kgmlToDataframe('ko00010.xml')
#' graph <- kgmlToDataframe('hsa00010.xml', TRUE, 'hsa')
#' }
#'
#' @importFrom KEGGgraph
#' @importFrom 'XML'
#'
#' @author
#' Igor Brandão

KGML2Graph <- function(kgml_, replaceOrg_=FALSE, orgToReplace_='') {
  # Get the pathway name
  pathway_name <- onlyNumber(kgml_)

  tryCatch({
    # Convert it into graph object
    mapkG <- KEGGgraph::parseKGML2Graph(kgml_, genesOnly=FALSE)

    # Get the node data
    aux <- names(mapkG@edgeData@data)

    # If node data is empty, use the edge data
    if (is.null(aux) | length(aux) == 0) {
      tryCatch({
        # Try another way to get the node data
        aux <- mapkG@nodes
      }, error=function(e) {
        return(NULL)
      })

      if (is.null(aux) | length(aux) == 0) {
        return(NULL)
      }
    }

    # Adjust the columns
    aux <- as.data.frame(aux, stringsAsFactors = FALSE)
    aux$node2 <- gsub("^.*\\|(.*)$", "\\1", aux$aux)
    colnames(aux)[1] <- "node1"
    aux$node1 <- gsub("^(.*)\\|.*$", "\\1", aux$node1)

    if (length(unlist(aux)) > 0) {
      # It means the organism has the pathway
      if (!replaceOrg_) {
        aux$org <- gsub("^([[:alpha:]]*).*$", "\\1", pathway_name)
      } else {
        aux$org <- orgToReplace_
      }

      aux$pathway <- gsub("^[[:alpha:]]*(.*$)", "\\1", pathway_name)
      return(aux)
    } else {
      # It means the organism doesn't have the pathway
      return(NULL)
    }
  }, error=function(e) {
    print(paste0('The pathway ', pathway_name, ' could no be converted. Skipping it...'))
    return(NULL)
  })
}

KGML2GraphDictionary <- function(kgml_, replaceOrg_=FALSE, orgToReplace_='') {

  # Load the dicionaty
  dictionary <- read.csv(file='./output/pathwaysDictionary/dictionary.csv', header=TRUE, sep=",", stringsAsFactors=FALSE)

  if (is.null(dictionary) | nrow(dictionary) == 0) {
    # Save the log file
    printLog(message_='The pathways nodes dictionary could not be found. Skipping it...',
             file_='KGML2GraphDictionary')

    return(FALSE)
  }

  # Create the KGML dataframe
  pathway <- KGML2Dataframe(kgml_)

  if (is.null(pathway) | isempty(pathway)) {
    # Save the log file
    printLog(message_='The pathway data could not be found. Skipping it...', file_='KGML2GraphDictionary')

    return(FALSE)
  }

  # Generate the hand-made graph with dictionary identifiers
  pathwayGraph <- data.frame(node1='', node2='', ec1='', entryID1='', reaction1='', x1='', y1='',
                             ec2='', entryID2='', reaction2='', x2='',  y2='', org='', pathway='', stringsAsFactors = FALSE)

  #**************************************##
  # Fill the pathway graph with entry IDs #
  #**************************************##

  rows <- nrow(pathway$edges)

  if (is.null(rows) | isempty(rows)) {
    return(NULL)
  } else {
    for (edgeIdx in 1:rows) {
      pathwayGraph[edgeIdx, 'node1'] <- pathway$edges[edgeIdx,]$entry1
      pathwayGraph[edgeIdx, 'node2'] <- pathway$edges[edgeIdx,]$entry2
    }
  }

  # Set basic info
  if (!replaceOrg_) {
    pathwayGraph['org'] <- 'ec'
  } else {
    pathwayGraph['org'] <- orgToReplace_
  }

  pathwayGraph['pathway'] <- pathway$pathwayinfo$number

  #**************************************************##
  # Convert the entry IDs according to the dictionary #
  #**************************************************##

  rows <- nrow(pathwayGraph)
  for (nodeIdx in 1:rows) {
    # Setup the current node
    currentNode1 <- c()
    currentNode2 <- c()

    # Get the info related to the current node
    currentNode1$entryID <- pathwayGraph[nodeIdx, 'node1'] # node1: entry ID
    currentNode2$entryID <- pathwayGraph[nodeIdx, 'node2'] # node2: entry ID
    currentNode1$ec <- pathway$nodes[pathway$nodes$entryID==currentNode1$entryID,]$name # node1: EC
    currentNode2$ec <- pathway$nodes[pathway$nodes$entryID==currentNode2$entryID,]$name # node2: EC
    currentNode1$reaction <- pathway$nodes[pathway$nodes$entryID==currentNode1$entryID,]$reaction # node1: reaction
    currentNode2$reaction <- pathway$nodes[pathway$nodes$entryID==currentNode2$entryID,]$reaction # node2: reaction
    currentNode1$x <- pathway$nodes[pathway$nodes$entryID==currentNode1$entryID,]$x # node1: graph x
    currentNode2$x <- pathway$nodes[pathway$nodes$entryID==currentNode2$entryID,]$x # node2: graph x
    currentNode1$y <- pathway$nodes[pathway$nodes$entryID==currentNode1$entryID,]$y # node1: graph y
    currentNode2$y <- pathway$nodes[pathway$nodes$entryID==currentNode2$entryID,]$y # node2: graph y

    #*********************************************##
    # Convert the entry IDs into the dictionary ID #
    #*********************************************##

    # Find the current node into the dictionary
    if (is.na(currentNode1$reaction)) {
      dictId1 <- dictionary[dictionary$x == currentNode1$x & dictionary$y == currentNode1$y, ]$id
    } else {
      dictId1 <- dictionary[dictionary$x == currentNode1$x & dictionary$y == currentNode1$y & dictionary$reaction == currentNode1$reaction, ]$id
    }

    if (is.na(currentNode2$reaction)) {
      dictId2 <- dictionary[dictionary$x == currentNode2$x & dictionary$y == currentNode2$y, ]$id
    } else {
      dictId2 <- dictionary[dictionary$x == currentNode2$x & dictionary$y == currentNode2$y & dictionary$reaction == currentNode2$reaction, ]$id
    }

    # Check if the dictionary contains the node, if the didctionary ID is empty, it means that
    # the node refers to a pathway connection (e.g:path:00020) or it is a compound
    if (is.null(dictId1) | isempty(dictId1)) {
      # Remove the current row
      pathwayGraph[nodeIdx,] <- NA
      next()
    } else {
      pathwayGraph[nodeIdx, 'node1'] <- dictId1
    }

    if (is.null(dictId2) | isempty(dictId2)) {
      # Remove the current row
      pathwayGraph[nodeIdx,] <- NA
      next()
    } else {
      pathwayGraph[nodeIdx, 'node2'] <- dictId2
    }

    #***************************##
    # Set graph other attributes #
    #***************************##

    pathwayGraph[nodeIdx, 'ec1'] <- currentNode1$ec
    pathwayGraph[nodeIdx, 'ec2'] <- currentNode2$ec
    pathwayGraph[nodeIdx, 'entryID1'] <- currentNode1$entryID
    pathwayGraph[nodeIdx, 'entryID2'] <- currentNode2$entryID
    pathwayGraph[nodeIdx, 'reaction1'] <- currentNode1$reaction
    pathwayGraph[nodeIdx, 'reaction2'] <- currentNode2$reaction
    pathwayGraph[nodeIdx, 'x1'] <- currentNode1$x
    pathwayGraph[nodeIdx, 'x2'] <- currentNode2$x
    pathwayGraph[nodeIdx, 'y1'] <- currentNode1$y
    pathwayGraph[nodeIdx, 'y2'] <- currentNode2$y
  }

  # Remove the NA rows
  if ( (any(is.na(pathwayGraph$reaction1)) | any(is.na(pathwayGraph$reaction2))) &
       (any(is.na(pathwayGraph$node1)) | any(is.na(pathwayGraph$node2))) ) {
    # Just the complete cases
    pathwayGraph <- pathwayGraph[complete.cases(pathwayGraph), ]

    # Reindex the dataframe
    if (nrow(pathwayGraph) > 0) {
      rownames(pathwayGraph) <- 1:nrow(pathwayGraph)
    } else {
      pathwayGraph <- NULL
    }
  }

  # Return the pathway graph
  return(pathwayGraph)
}

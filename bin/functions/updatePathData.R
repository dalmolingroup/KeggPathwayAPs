#This file contains the functions that update the data 
#for the pathData and export it to MySQL
#cria db Mysql

rm(list=ls(all=TRUE))



correctPathKeggData<-function(){
  #use this function just after loading the original data from KEGG
  
  # get data to be correct
  sql<-'select id, code from pathData'
  teste<-dbGetQuery(dbCon,sql)
  
  # get data from our database
  sql<-'select pId, pName from path'
  teste2<-dbGetQuery(dbCon,sql)
  
  teste2$pName<-sub(x = teste2$pName,pattern = 'ec',replacement = '')
  pname<-paste0('"',teste2$pName,'"')
  pname <- do.call(paste, c(as.list(pname), sep = ","))
  
  #remove all pathways not present in our database
  sql<-paste0('delete from pathData where code not in (',pname,')')
  dbExecute(dbCon,sql)
  
  #check if ids from KEGG are iqual our pId
  teste3<-merge(teste,teste2,by.x = 'id',by.y = 'pId')
  teste3[teste3$code != teste3$pName,]
  
  
  idx = 1
  #update all ids
  for(idx in 1:nrow(teste2)){
    sql<-paste0('update pathData set id = ',teste2$pId[idx],' where code = \'',teste2$pName[idx],'\';' )
    dbExecute(dbCon,sql)
    
  }
  
  
  
}


getAPImpact <- function(g, verbose_=FALSE) {
  #changed by Clovis Reis
  
  # Status message
  if (verbose_) {
    print("Calculating the graph articulation points...")
  }
  
  # Calculate the articulation points
  articulation_points <- as.integer(getGraphBottleneck(g))
  
  # Status message
  if (verbose_) {
    print("Calculating the articulation points impact...")
  }
  retorno<-list()
  if (!is.null(articulation_points) && length(articulation_points) > 0) {
    # Unify the graph nodes and set its community
    result <- data.frame(ap=as.integer( articulation_points), 
                         noComponents=0, 
                         componentsSize="0", 
                         impact=0, 
                         weightedImpact=0, 
                         stringsAsFactors = FALSE)
    
    # Dismantle the graph to calculate its impact
    idx=1
    for (idx in 1:length(articulation_points)) {
      # Set the current articulation point
      currentAP <- articulation_points[idx]
      
      # Set a temp graph without the current articulation point
      tempGraph <- delete_vertices(g, as.character(currentAP))
      
      # Calculate the number and size of the disconnected components
      components <- components(tempGraph)
      result$noComponents[idx] <- components$no
      result$componentsSize[idx] <- paste(components$csize, collapse = ', ')
      
      # Calculate the AP impact based on: Gabriele Farina paper
      result$impact[idx] <- min(components$csize)
      
      # Calculate the AP weighted impact from original creation
      result$weightedImpact[idx] <- (min(components$csize) * components$no)
      result$sd[idx] <- sd(components$csize)
      result$maxNet[idx]<-max(components$csize)
      
      rm(tempGraph)
    }
    
    #compared impact 
    minSd<-min(result$sd)
    impact<-mean(result$maxNet[result$sd == minSd])
    retorno[[1]]<- impact
    
    #nodes with min impact
    nodes<-result$ap[result$sd == minSd]
    
    #vertex_attr(g,'nId')%in%impact
    # label of min impart nodes, comma separated
    retorno[[2]]<-paste(vertex_attr(g,'eName')[vertex_attr(g,'nId')%in%nodes], collapse = ', ')
    
  }else {
    retorno[[1]]<- 1
    retorno[[2]]<- 'NA'
  }
  return(retorno)
}

# # SQLite
# if (!requireNamespace("DBI", quietly = TRUE))
#   install.packages("DBI", repos = "http://cran.us.r-project.org")
# library(DBI)
# 
# # tidyr
# if (!requireNamespace("tidyr", quietly = TRUE))
#   install.packages("tidyr", repos = "http://cran.us.r-project.org")
# library(tidyr)
# 
# # tidyverse
# if (!requireNamespace("tidyverse", quietly = TRUE))
#   install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# library(tidyverse) 
# 
# if (!requireNamespace("RMySQL", quietly = TRUE))
#   install.packages("RMySQL", repos = "http://cran.us.r-project.org")
# library(RMySQL)
# 

dirBase<<-"/home/clovis/Doutorado/Projetos/Igor/gitAPs"
#bin dir
binDir<<-file.path(dirBase,"bin")
#function dir
funcDir<<-file.path(binDir,"functions")
#database folder and file
dbDir<<-file.path(dirBase,"data","database")
dbTemplate <- file.path(dbDir,"APs.sql")
dbFile<<-file.path(dbDir,"dictionary.db")

source(file.path(funcDir,"dependencies.R"))
loadDependencies()

#mysql connection
#mydbCon = dbConnect(MySQL(), user='apsapp', password='1234!@#$qwer', dbname='APs', host='127.0.0.1')
#rs = dbSendQuery(mydb, "select * from some_table")

#sqlite connection
#conect and test dictionary
dbCon <<- dbConnect(RSQLite::SQLite(), dbFile)

#select all pathways
sql<-"SELECT DISTINCT pId, pName FROM path"
pathways <- dbGetQuery(dbCon,sql)
#remove ec string
pathways$name<-sub(pattern = 'ec',
                    replacement = '',
                    x = pathways$pName)

#counting parameters
# "eukaryotes_count"	
# "prokaryotes_count"	
# "animals_count"	
# "plants_count"	
# "fungi_count"	
# "protists_count"	
# "bacteria_count"	
# "archaea_count"	
# "total_species"	
sql<- "SELECT pId,
          	sum(case when o.taxon = \'Eukaryotes\' then 1 else 0 end) as euk,
          	sum(case when o.taxon = \'Prokaryotes\' then 1 else 0 end) prok,
          	sum(case when o.reino = \'Animals\' then 1 else 0 end) animal,
          	sum(case when o.reino = \'Plants\' then 1 else 0 end) plant,
          	sum(case when o.reino = \'Fungi\' then 1 else 0 end) fungi,
          	sum(case when o.reino = \'Protists\' then 1 else 0 end) prot,
          	sum(case when o.reino = \'Bacteria\' then 1 else 0 end) bact,
          	sum(case when o.reino = \'Archaea\' then 1 else 0 end) arch,
          	count(*) tot
          FROM (SELECT DISTINCT org, pId
          		from nodebyorgs) op INNER JOIN
          		organism as o on o.orgId = op.org
          GROUP BY pId"
orgCounts<-dbGetQuery(dbCon,sql)


#counting parameters
# "nodes"	
# "ap_number"	
# "others_number"	
# "mean_degree"	
# "mean_betweenness"
sql<- "SELECT pId,
            count(*) as nodes,
          	sum(n.isAP) as aps,
          	sum(case when n.isAP = 0 then 1 else 0 end) others,
          	avg(degree) mdegree,
          	avg(betweenness) mbetwe
          FROM nodemetric n
          GROUP BY pId"
nodeCounts<-dbGetQuery(dbCon,sql)

#repeat for each pathway
# "hub_number"	
# "hap_number"	
# "edges"	
# "node_highest_impact"
# "disconnected_nodes"	
# "community"	
idx = 1
for(idx in 1:nrow(pathways)){
  pname<-pathways$pName[idx]
  pId<-pathways$pId[idx]
  nodeNr<-nodeCounts$nodes[nodeCounts$pId == pId]
  percent<-round(nodeNr*20/100,0)

  #"hub_number"	
  #"hap_number"	
  sql<-paste0("select nId, isAP from nodemetric where pId = ", pId," limit ",percent)
  tmp<-dbGetQuery(dbCon,sql)
  hub_number<- nrow(tmp)
  hap_number<- nrow(tmp[tmp$isAP ==1,])

  
  g<-getGraphFromPath(pname,removeFake = T)[[2]]

  # "edges"	
  edges<-length(E(g))
  
  # "community"	
  community <- length(groups(cluster_walktrap(g)))

  # "node_highest_impact"
  # "disconnected_nodes"	
  impact<-getAPImpact(g)
  node_highest_impact <- impact[[2]]
  disconnected_nodes <- impact[[1]]
  
  sql<-paste0('update pathData 
              set nodes = ',nodeCounts$nodes[nodeCounts$pId == pId],', ',
                  'mean_degree = ',nodeCounts$mdegree[nodeCounts$pId == pId],', ',
                  'mean_betweenness = ',nodeCounts$mbetwe[nodeCounts$pId == pId],', ',
                  'ap_number = ',nodeCounts$aps[nodeCounts$pId == pId],', ',
                  'hub_number = ',hub_number,', ',
                  'hap_number = ',hap_number,', ',
                  'others_number = ',nodeCounts$others[nodeCounts$pId == pId],', ',
                  'eukaryotes_count = ',orgCounts$euk[orgCounts$pId == pId],',',
                  'prokaryotes_count = ',orgCounts$prok[orgCounts$pId == pId],',',
                  'animals_count = ',orgCounts$animal[orgCounts$pId == pId],',',
                  'plants_count = ',orgCounts$plant[orgCounts$pId == pId],',',
                  'fungi_count = ',orgCounts$fungi[orgCounts$pId == pId],',',
                  'protists_count = ',orgCounts$prot[orgCounts$pId == pId],',',
                  'bacteria_count = ',orgCounts$bact[orgCounts$pId == pId],',',
                  'archaea_count = ',orgCounts$arch[orgCounts$pId == pId],',',
                  'total_species = ',orgCounts$tot[orgCounts$pId == pId],',',
                  'edges = ',edges,',',
                  'node_highest_impact = \'',node_highest_impact,'\',',
                  'disconnected_nodes = ',disconnected_nodes,',',
                  'community = ',community,
              
              ' where id = ', pId)
  dbExecute(dbCon,sql)
}





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
  #   edit lstSomeOrgs file for personalized organims
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

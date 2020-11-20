#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#********************************#
# Package with general functions #
#********************************#

# ---- IMPORT SECTION ----

# helperFunctions.R #

# Import the necessary libraries
library(stringr) # regex manipulation
library(pracma) # string manipulation

#*******************************************************************************************#

# ---- MESSAGE HANDLER ----

#' Function to generate dynamic messages
#'
#' @param message_ String representing the content of the message.
#'
#' @return This function does not return nothing, just export files.
#'
#' @examples
#' \dontrun{
#' printMessage('Hello World!')
#' }
#'
#' @author
#' Igor Brandão

printMessage <- function(message_) {
  cat("\n")
  print("------------------------------------------------")
  print(message_)
  print("------------------------------------------------")
  cat("\n")
}

#*******************************************************************************************#

# ---- GRAPH HANDLER ----

#' Function to apply attributes into iGraph objects
#'
#' @param graph_ iGraph object.
#' @param default_ String representing the default value of the attribute.
#' @param valNodeList_ List of values to apply into the graph nodes.
#'
#' @return This function does not return nothing, just export files.
#'
#' @examples
#' \dontrun{
#' nAttrs$fillcolor <- makeAttr(graph, "lightblue", list(orange=toprbccName))
#' }
#'
#' @author
#' Igor Brandão

makeAttr <- function(graph_, default_, valNodeList_) {
  tmp <- nodes(graph_)
  x <- rep(default_, length(tmp)); names(x) <- tmp

  if(!missing(valNodeList_)) {
    stopifnot(is.list(valNodeList_))
    allnodes <- unlist(valNodeList_)
    stopifnot(all(allnodes %in% tmp))
    for(i in seq(valNodeList)) {
      x[valNodeList_[[i]]] <- names(valNodeList_)[i]
    }
  }
  return(x)
}

#*******************************************************************************************#

# ---- STRING HANDLER ----

#' Function to perform trim operation
#'
#' @param str_ The string to be trimmed.
#'
#' @return Returns string w/o leading or trailing whitespace
#'
#' @examples
#' \dontrun{
#'  myDummy$country <- trim(myDummy$country)
#' }
#'
#' @author
#' Igor Brandão

trim <- function (str_) {
  gsub("^\\s+|\\s+$", "", str_)
}

#' Function to remove all non-numerical characters from a string
#'
#' @param str_ The string to be handled.
#'
#' @return Returns string without non-numerical characters
#'
#' @examples
#' \dontrun{
#'  myString <- onlyNumber(myString)
#' }
#'
#' @author
#' Igor Brandão

onlyNumber <- function (str_) {
  temp <- gregexpr("[0-9]+", str_)
  str_ <- unlist(regmatches(str_, temp))
  return(str_)
}

#*******************************************************************************************#

# ---- LOG HANDLER ----

#' Function to generate dynamic logs
#'
#' @param message_ Message to be written in the log file
#' @param file_ Filename withou extension
#' @param folder_ Folder name
#'
#' @return This function does not return nothing, just export files.
#'
#' @examples
#' \dontrun{
#' printLog('Error xyz', 'xpto')
#' printLog('Error xyz', 'xpto', log)
#' }
#'
#' @author
#' Igor Brandão

printLog <- function(message_, file_, folder_='log') {
  # Status message
  printMessage(paste0('LOG: ', message_))

  # Save the log file
  if (!dir.exists(file.path(paste0('./', folder_, '/')))) {
    dir.create(file.path(paste0('./', folder_, '/')), showWarnings = FALSE, mode = "0775")
  }

  # Generate the log file
  write(message_, file=paste0('./', folder_, '/', format(Sys.time(), "%Y%m%d_%H%M%S_"), file_, '.txt'))
}


createOrgList <- function(funcDir, orgList){
  lstSomeOrgs <- file.path(funcDir,"lstSomeOrgs")
  cat(file = lstSomeOrgs, 
      '# Put here the list of organisms to download\n',
      'declare -a ORG_LIST=(', orgList,')\n')
}
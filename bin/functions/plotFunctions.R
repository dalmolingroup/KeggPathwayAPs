#*************************************************
#   ATTENTION!!! This code is provided "AS-IS", 
#   with no warranties, express or implied, and 
#   hereby disclaims all implied warranties, 
#   including any warranty of merchantability and 
#   warranty of fitness for a particular purpose. 
#   It's released under GPL v2.
#*************************************************

#******************************************#
# All plot and figures functions           #
#******************************************#

# plotFunctions.R #

# ---- IMPORT SECTION ----

#' This is the set of functions to handle the KEGG kgml files
#'
#' @author
#' Igor Brandão / Clovis Reis


plotEnzymeDistrib <- function(sdFactor = 1){
  orgCounts<-getOrgCounts()
  
  md<-mean(orgCounts$count)
  sd <- sd(orgCounts$count)
  
  hist(orgCounts$count)
  nrow(orgCounts[orgCounts$count>md-sdFactor*sd,])/nrow(orgCounts)
  
  
}

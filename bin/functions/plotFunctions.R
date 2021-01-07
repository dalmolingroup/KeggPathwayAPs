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


enzymeDistrib <- function(sdFactor = 1,
                          plot = T, 
                          save = T,
                          quiet = T){
  orgCounts<-getOrgCounts()
  
  md<-mean(orgCounts$count)
  sd <- sd(orgCounts$count)
  
  #density calculation
  d <- density(orgCounts$count,n=100)
  densidade<-data.frame(cbind(d$x,d$y))
  colnames(densidade) <- c("xDens","yDens")
  
  
  h<-hist(orgCounts$count,breaks = 100,plot = F)
  maxH<-max(h$counts)
  minH<-min(h$counts)
  maxD<-max(densidade$y)
  minD<-min(densidade$y)
  
  densidade$yDens<-(densidade$yDens-minD)/
                    (maxD-minD)*maxH
  limit<-md-sdFactor*sd
  percent<-round(nrow(orgCounts[orgCounts$count>md-sdFactor*sd,])/nrow(orgCounts)*100,2)
  percent2<-round(100-percent,2)
  if(plot){
    # plot ----
    g1<-ggplot()+
      ggtitle(paste0("Enzymes distribution per Organims"))+
      theme_bw()+
      xlab('Enzymes count')+
      ylab("Organim count") +
      scale_y_continuous(name = expression("Organim count"))+
      geom_rect(aes(xmin = min(densidade$xDens), xmax = limit,
                    ymin = 0, ymax = maxH+10,
                    fill = "Discarted"))+
      geom_rect(aes(xmin = limit, xmax = max(densidade$xDens),
                    ymin = 0, ymax = maxH+10,
                    fill = "Considered"))+
      geom_vline(xintercept = md, 
                 col = alpha("blue",0.45))+
      geom_vline(xintercept = limit, 
                 col = alpha("blue",0.45),
                 lty = 2)+
      geom_line(data=densidade, 
                aes(xDens,
                    yDens,
                    linetype="Density",
                    color="Density"))+
      annotate(geom = 'text',
               x=c(md-15,
                   limit-15),
               y=c(maxH-10,
                   maxH-10),
               label=c('Mean',
                       paste0(sdFactor,' Std dev')),
               size = c(3,3),
               angle = 90)+
      annotate(geom = 'label',
               x=c((max(densidade$xDens)+limit)/2,
                   (min(densidade$xDens)+limit)/2,
                   max(densidade$xDens)-10),
               y=c(maxH,
                   maxH,
                   maxH/2),
               label=c(paste0(percent,'%'),
                       paste0(percent2,'%'),
                       paste0('Total: ',nrow(orgCounts),
                              ' organisms')),
               size = c(3,3,3),
               hjust = c('top',
                         'left',
                         'right'))+
      scale_fill_manual("",values = c("Discarted"=alpha("magenta2",0.15),
                                      "Considered"=alpha("green",0.15)))+
      scale_color_manual("",guide=F,values = c("Density"= "orange"))+
      scale_linetype_manual("",values = c("Density"=1))+
      guides(shape = guide_legend(override.aes = list(shape = c(25,21), 
                                                      fill=c("magenta2",NA), 
                                                      color= c("magenta2", "black"))),
             linetype = guide_legend(override.aes = list(color = "orange")))
    plot(g1)
    # save ----
    if(save){
      fileName=file.path(dirFig,'enzDistr.pdf')
      ggsave(filename = fileName, 
             plot = g1,
             device = "pdf",
             width = 11,height = 8,
             dpi=600)
    }
  }
  # quiet ----
  if(!quiet){
    cat('Total of organisms:', nrow(orgCounts),'\n',
        '\tMean of enzymes counts:', round(md,2),
        '\tStd Dev:', round(sd,2),'\n',
        '\tOrgs considered:',nrow(orgCounts[orgCounts$count>md-sdFactor*sd,]),
        '\t(',percent,'%)')
  }
  return(orgCounts$org[orgCounts$count>md-sdFactor*sd])
}

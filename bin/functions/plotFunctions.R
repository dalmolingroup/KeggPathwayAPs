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
    annotate('text',x=c(md+8,limit+8,max(densidade$xDens)-10,min(densidade$xDens)+10),
                  y=c(maxH-10,maxH-10,maxH/2,maxH/2),
                  label=c('Mean',
                          paste0(sdFactor,' Std dev'),
                          paste0(percent,'% of organisms'),
                          paste0(percent2,'% of organisms')),
                  #angle = c(90,90),
                  size = c(3,3,3,3),
             hjust = c('left','left','right','left'))+
    # geom_point(data=histTodos, aes(x=breaks, 
    #                                y=(counts*max(densidade$yDens)/ max(counts)),
    #                                color= "Samples count",
    #                                shape="Samples count"))+
    # geom_point(data = coordPicos,aes(x,
    #                                  y,
    #                                  shape="Peaks", 
    #                                  color= "Peaks",
    #                                  fill = "Peaks"))+
    scale_fill_manual("",values = c("Discarted"=alpha("magenta2",0.15),
                                            "Considered"=alpha("green",0.15)))+
    # scale_shape_manual("",values = c("Samples count"=21,
    #                                  "Peaks"=25))+
    scale_color_manual("",guide=F,values = c("Mean"=alpha("blue",1),
                                             "SD"=alpha("blue",1), 
                                             "Density"= "orange"))+
    scale_linetype_manual("",values = c("Density"=1,
                                        "Mean" = 1,
                                        "SD" = 2))+
    guides(shape = guide_legend(override.aes = list(shape = c(25,21), 
                                                    fill=c("magenta2",NA), 
                                                    color= c("magenta2", "black"))),
           linetype = guide_legend(override.aes = list(color = "orange")))
  
  fileName=file.path(dirFig,'enzDistr.pdf')
  ggsave(filename = fileName, 
         plot = g1,
         device = "pdf",
         width = 11,height = 8,
         dpi=600)
  
  
}

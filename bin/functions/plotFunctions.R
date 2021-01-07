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


enzymeDistrib <- function(sdFactor = NA,
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
  if(is.na(sdFactor)){
    limit <- 0
    txtSd <-'Std dev'
    posSd <- md-sd
    percent<-100
    percent2<-0 
  }else{
    limit<-md-sdFactor*sd
    txtSd <-paste0(sdFactor,' Std dev')
    posSd <- limit
    percent<-round(nrow(orgCounts[orgCounts$count>md-sdFactor*sd,])/nrow(orgCounts)*100,2)
    percent2<-round(100-percent,2)
  }
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
      geom_vline(xintercept = posSd, 
                 col = alpha("blue",0.45),
                 lty = 2)+
      geom_line(data=densidade, 
                aes(xDens,
                    yDens,
                    linetype="Density",
                    color="Density"))+
      annotate(geom = 'text',
               x=c(md-15,
                   md-sd-15),
               y=c(maxH-10,
                   maxH-10),
               label=c('Mean',
                       txtSd),
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
        '\tOrgs considered:',nrow(orgCounts[orgCounts$count>=limit,]),
        '\t(',percent,'%)')
  }
  return(orgCounts$org[orgCounts$count>=limit])
}


plotHeatMap <- function(apCounts,
                        save = T){
  
  step = 0.0005
  faixas <- list()
  idx <- 1
  for (faixa in seq(0, 0.95, step)) {
    Ap <- nrow(apCounts[apCounts$percentage > faixa &
                           apCounts$percentage <= faixa + .05 &
                           apCounts$isAP == 1, ])
    nAp <- nrow(apCounts[apCounts$percentage > faixa & 
                            apCounts$percentage <= faixa + 0.5 &
                            apCounts$isAP == 0, ])
    
    faixas[[idx]]<-data.frame(percent = faixa + 0.05,
                              Ap = Ap,
                              nAp = nAp,
                              sig=0, 
                              stringsAsFactors=F)
    
    idx<-idx+1
    #print(faixa)
  }
  faixas <- do.call(rbind, faixas)
  
  faixas$ApN <- (faixas$Ap-min(faixas$Ap)) / (max(faixas$Ap)-min(faixas$Ap))
  faixas$nApN <- (faixas$nAp - min(faixas$nAp)) / (max(faixas$nAp)-min(faixas$nAp))
  
  
  faixas <- faixas[faixas$percent>0.15,]
  faixas$ApN <- (faixas$Ap-min(faixas$Ap)) / (max(faixas$Ap)-min(faixas$Ap))
  faixas$nApN <- (faixas$nAp - min(faixas$nAp)) / (max(faixas$nAp)-min(faixas$nAp))
  
  g1 <- ggplot(data = faixas) +
    geom_tile(aes(y=percent, x=1, fill=(ApN))) +
    geom_tile(aes(y=percent, x=2), fill='white', color='white') +
    geom_tile(aes(y=percent, x=3, fill=(nApN))) +
    
    scale_fill_gradient(low = "white", high = "#173F5F") +
    #scale_fill_gradient(low = "white", high = "black") +
    #scale_fill_gradientn(colours = terrain.colors(10)) +
    
    geom_hline(yintercept=0.75, linetype='dashed', color="#E75480", size=1) +
    # Chart visual properties
    xlab("") +
    ylab("") +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_text(face="bold", size=20, hjust = 0),
          axis.title.x = element_text(face="bold", size=20, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size=16),
          axis.title.y = element_text(face="bold", size=20, margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.text.y = element_text(size=16),
          legend.title = element_text(face="bold", size=18),
          legend.text = element_text(size=16),
          legend.position = 'right') + labs(fill = "Density")
  plot(g1)
  if(save){
    fileName=file.path(dirFig,'heatMap.pdf')
    ggsave(filename = fileName, 
           plot = g1,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
  }}

plotBinomial <- function(apCounts, 
                         p_value = 0.01,
                         interval = 0.10,
                         save = T){
  
  #**************************************************************************##
  # Apply a normalization:                                              #
  # 100% of occurrence in a pathway can be compared with 50% of other pathway #
  #**************************************************************************##
  
  # Get the unique pathways
  uniquePathways <- unique(apCounts$pName)
  
  # Calculates the normalized frequency
  for (item in uniquePathways) {
    # Max frequency in a pathway
    pathwayMaxFrequency <- max(apCounts$percentage[apCounts$pName==item])
    
    # Min frequency in a pathway
    pathwayMinFrequency <- min(apCounts$percentage[apCounts$pName==item])
    
    # Normalized frequency for each protein
    apCounts$nPercent[apCounts$pName==item] <-
      (apCounts$percentage[apCounts$pName==item]-pathwayMinFrequency)/
      (pathwayMaxFrequency-pathwayMinFrequency)
  }
  
  # Fix for NAN cases (when min and max frequency have the same values)
  apCounts$nPercent[is.nan(apCounts$nPercent)] <- apCounts$percentage[is.nan(apCounts$nPercent)]/100
  
  # Filter dataSet from proteins with ZERO frequency
  apCounts <- apCounts[!apCounts$occurrences==0,]
  
  # Order the dataSet
  apCounts <- apCounts[order(apCounts$nPercent, decreasing = T),]
  
  
  #*****************************************##
  # Generate the hypergeometric distribution #
  #*****************************************##
  
  # Count the bottlenecks and non-bottlenecks
  countsBase <- c(APs=nrow(apCounts[apCounts$isAP ==1,]), 
                  nAPs=nrow(apCounts[apCounts$isAP !=1,]))
  
  proporcao<- countsBase[1]/(countsBase[1]+countsBase[2])
  
  ranges<-seq(0,1.1,interval)
  
  # Create a dataFrame for the result
  distribution <- data.frame(ini=0,
                             range=0,
                             AP=0,
                             nAP=0,
                             binom=NA,
                             stringsAsFactors = F)
  # Loop over all dataSet
  idx=100
  for (idx in 1:(length(ranges)-1)) {
    # Set the cumulative range [initVal:range]
    initVal <- ranges[idx]
    range<-ranges[idx+1]
    
    # Temporaly dataFrame for indexed results
    countsTop <- data.frame(ini=numeric(),
                            range=numeric(),
                            AP=numeric(),
                            nAP=numeric(),
                            binom=numeric(),
                            stringsAsFactors = F)
    
    # Retrieve the cumulative proteins
    top <- apCounts[apCounts$nPercent>=ranges[idx]&
                      apCounts$nPercent<ranges[idx+1], ]
    
    # Number of draws
    drawn <- nrow(top)
    
    # The number of articulation points in the accumulated group
    btn <- nrow(top[top$isAP == 1,])
    nbtn <- nrow(top[top$isAP == 0,])
    
    if((btn+nbtn) != 0){
      binom<-binom.test(x = btn,
                        n = (btn+nbtn),
                        p = proporcao,
                        alternative = "g")
      binom<-binom$p.value
    }else{
      binom<-1
    }
    
    countsTop[1,] <- t(c(initVal, range-0.001, btn,nbtn,binom))

    # Bind each result
    distribution <- rbind(distribution, countsTop)
    initVal <- range+1
  }

  # Adjust the p-value in order to compensate the accumulated error with
  # Benjamini-Hochberg method
  distribution$pCor <- p.adjust(distribution$binom, method = "BH")
  distribution<-na.exclude(distribution)
  
  # plot ----
  
  # Classify the articulation points
  distribution$group <-''
  distribution[distribution$pCor<=p_value,]$group <- 
    rep('Significative',
        nrow(distribution[distribution$pCor<=p_value,]))
  distribution[distribution$pCor>p_value,]$group <- 
    rep('Non-Significative',
        nrow(distribution[distribution$pCor>p_value,]))
  
  
  # Plot the graph
  g1 <- ggplot() + ggtitle("") + # for the main title
    xlab("Range (%)") +
    ylab("Ratio (Articulation point/Total)") +
    geom_col(data = distribution, 
             aes(x=range-.1, 
                 y=(AP/(AP+nAP)), 
                 fill=group), 
             width=.09) +
    geom_hline(yintercept = proporcao, lty=2, col="#E75480") +
    scale_fill_manual(values = c("#ED553B", "#173F5F")) +
    theme_bw() +
    theme(plot.title = element_text(face="bold", size=20, hjust = 0),
          axis.title.x = element_text(face="bold", size=20, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size=16),
          axis.title.y = element_text(face="bold", size=20, margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.text.y = element_text(size=16),
          legend.title = element_text(face="bold", size=18),
          legend.text = element_text(size=16),
          legend.position = 'none') + labs(fill = "Range group")
  
  plot(g1)
  if(save){
    fileName=file.path(dirFig,'binomial.pdf')
    ggsave(filename = fileName, 
           plot = g1,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
  }
  
}


# FIM ----

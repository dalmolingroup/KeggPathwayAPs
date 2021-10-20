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
                          quiet = T,
                          bicaudal = F,
                          type = NA,
                          value = NA){
  if(is.na(value)){
    value <- 'All Organisms'
  }
  orgCounts<-getOrgCounts(type = type,
                          value = value)
  
  md<-mean(orgCounts$count)
  sd <- sd(orgCounts$count)
  max <- max(orgCounts$count)
  
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
    if(bicaudal){
      limit<-md-sdFactor*sd
      limitMax<-md+sdFactor*sd
      txtSd <-paste0(sdFactor,' Std dev')
      posSd <- limit
      posSdMax<-limitMax
      percent<-round(nrow(orgCounts[orgCounts$count>md-sdFactor*sd & orgCounts$count<md+sdFactor*sd ,])/nrow(orgCounts)*100,2)
      percent2<-round(100-percent,2)
    }else{
      limit<-md-sdFactor*sd
      limitMax<-max
      txtSd <-paste0(sdFactor,' Std dev')
      posSd <- limit
      percent<-round(nrow(orgCounts[orgCounts$count>md-sdFactor*sd,])/nrow(orgCounts)*100,2)
      percent2<-round(100-percent,2)
    }
  }
  if(plot){
    # plot ----
    g1<-ggplot()+
      ggtitle(paste0("Enzymes distribution per Organims - ",value))+
      theme_bw()+
      xlab('Enzymes count')+
      ylab("Organim count") +
      geom_rect(aes(xmin = min(densidade$xDens), xmax = limit,
                    ymin = 0, ymax = maxH+10,
                    fill = "Discarted"))+
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
                   md-sdFactor*sd-15),
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
    if(bicaudal){
      g1<-g1+geom_rect(aes(xmin = limitMax, xmax = max(densidade$xDens),
                    ymin = 0, ymax = maxH+10,
                    fill = "Discarted"))+
        geom_rect(aes(xmin = limit, xmax = limitMax,
                      ymin = 0, ymax = maxH+10,
                      fill = "Considered"))+
        geom_vline(xintercept = posSdMax, 
                   col = alpha("blue",0.45),
                   lty = 2)
    }else{
      g1<-g1+geom_rect(aes(xmin = limit, xmax = max(densidade$xDens),
                    ymin = 0, ymax = maxH+10,
                    fill = "Considered"))
    }
        
  }
  if(value == "All Organisms"){
    #g1<-g1+scale_x_continuous(limits = c(0,2000))
    idx<-1
    groups<-list()
    bins<-seq(from=0, to= 2000, by=250)
    for (bin in (1:(length(bins)-1))) {
      for(taxon in c('Archaea','Bacteria','Eukaryotes')){
        qtd<-nrow(orgCounts[orgCounts$taxon == taxon &
                            (orgCounts$count>bins[bin] &
                             orgCounts$count<=bins[bin+1]),])
        groups[[idx]]<-data.frame(taxon = taxon,
                                  bin = bin,
                                  count = qtd,
                                  ini=bins[bin],
                                  fim = bins[bin+1],
                                  stringsAsFactors=F)
        
        idx<-idx+1
        #print(group)
      }
      }
    groups <- do.call(rbind, groups)
    groups$position<- (groups$bin-1)*250+125
    
    g2<-ggplot(groups, aes(fill=taxon, y=count, x=position)) + 
      theme_bw()+
      #theme_nothing()+
      xlab('Proportion')+
      ylab("") +
      geom_bar(position = 'fill',stat="identity")+
      scale_x_continuous(limits = c(-150,2000),labels = NULL, breaks = NULL)+
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            axis.line.x = element_blank(),
            axis.line.x.top = element_blank(),
            axis.line.x.bottom = element_blank(),
            # axis.line.y = element_blank(),
            # axis.line.y.left = element_blank(),
            # axis.line.y.right = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            )
      
    
    
    # grid.newpage()
    # grid.draw(rbind(ggplotGrob(g1), ggplotGrob(g2), size = "last"))
    # 
    # g2
    # g3<-g1
    g1<-plot_grid(g1, g2, ncol = 1, align = 'v',rel_heights=c(3,1))
  }
    plot(g1)
    # save ----
    if(save){
      fileName=file.path(dirFig,paste0(value,'enzDistr.pdf'))
      ggsave(filename = fileName, 
             plot = g1,
             device = "pdf",
             width = 11,height = 8,
             dpi=600)
    }
  
  # quiet ----
  if(!quiet){
    cat('Total of organisms:', nrow(orgCounts),'\n',
        '\tMean of enzymes counts:', round(md,2),
        '\tStd Dev:', round(sd,2),'\n',
        '\tOrgs considered:',nrow(orgCounts[orgCounts$count>=limit & orgCounts$count<=limitMax,]),
        '\t(',percent,'%)')
  }
  return(orgCounts$org[orgCounts$count>=limit & orgCounts$count<=limitMax])
}


plotHeatMap <- function(apCounts,
                        normalized = T,
                        save = T, 
                        value = NA){
  if(is.na(value)){
    value <- 'All Organisms'
  }
  if(normalized){
    apCounts$percent<-apCounts$nPercent
  }else{
    apCounts$percent<-apCounts$percentage
  }
  step = 0.0005
  faixas <- list()
  idx <- 1
  for (faixa in seq(0, 0.95, step)) {
    Ap <- nrow(apCounts[apCounts$percent > faixa &
                           apCounts$percent <= faixa + .05 &
                           apCounts$isAP == 1, ])
    nAp <- nrow(apCounts[apCounts$percent > faixa & 
                            apCounts$percent <= faixa + 0.5 &
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
  
  label= factor(c('APs','nAP'),ordered = F)
  g1 <- ggplot(data = faixas) +
    geom_tile(aes(y=percent, x=label[1], fill=(ApN))) +
    #geom_tile(aes(y=percent, x=2), fill='white', color='white') +
    geom_tile(aes(y=percent, x=label[2], fill=(nApN))) +
    
    scale_fill_gradient(low = "white", high = "#173F5F") +
    #scale_fill_gradient(low = "white", high = "black") +
    #scale_fill_gradientn(colours = terrain.colors(10)) +
    
    geom_hline(yintercept=0.75, linetype='dashed', color="#E75480", size=1) +
    # Chart visual properties
    xlab("") +
    ylab("") +
    ggtitle(value) +
    #scale_x_discrete(' ',labels= c('bucets','m','jj'))+
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
    fileName=file.path(dirFig,paste0(value,'heatMap.pdf'))
    ggsave(filename = fileName, 
           plot = g1,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
  }
  }

plotBinomial <- function(distribution,
                         proportion,
                         alternative = 'g',
                         p_value = 0.01,
                         save = T,
                         quiet = T,
                         value = NA){
  if(is.na(value)){
    value <- 'All Organisms'
  }
  distribution$binom <- 1
  for (idx in 1:nrow(distribution)) {
    AP <- distribution$AP[idx]
    nAP <- distribution$nAP[idx]
    if((AP+nAP) != 0){
      binom<-binom.test(x = AP,
                        n = (AP+nAP),
                        p = proportion,
                        alternative = alternative)
      binom<-binom$p.value
    }else{
      binom<-1
    }
    
    distribution$binom[idx] <- binom
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
  yMax<-max(distribution$AP/(distribution$AP+distribution$nAP))
  
  colwidth<-1/nrow(distribution)-(1/(nrow(distribution)*10))
  # Plot the graph
  g1 <- ggplot() + ggtitle(value) + # for the main title
    xlab("Range (%)") +
    ylab("Ratio (Articulation point/Total)") +
    geom_col(data = distribution, 
             aes(x=range-.05, 
                 y=(AP/(AP+nAP)), 
                 fill=group), 
             width=colwidth) +
    geom_hline(yintercept = proportion, lty=2, col="#E75480") +
    scale_fill_manual(values = c("#ED553B", "#173F5F")) +
    annotate("text", x = 0.05, y = -0.01, label = "Rare",size = 6, col='blue') +  
    annotate("text", x = 0.95, y = -0.01, label = "Common",size = 6, col='blue') +  
    # geom_text(data = data_frame(label=c('Rare','Common'),
    #                             x = c(0,1),
    #                             y = c(-0.5,-0.5)),
    #           aes(label = label, x=x, y=y),
    #           hjust = 0,
    #           size = 8) +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0,yMax),
                    clip = 'off') +   # This keeps the labels from disappearing
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
  # save ----
  if(save){
    fileName=file.path(dirFig,paste0(value,'binomial.pdf'))
    ggsave(filename = fileName, 
           plot = g1,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
    cat('passou')
  }
  # quiet ----
  if(!quiet){
    cat('range\tAP\tnAP\tp-val\t\tp corr\n')
    idx=1
    for (idx in 1:nrow(distribution)) {
      cat(distribution$ini[idx],'\t',
          distribution$AP[idx],'\t',
          distribution$nAP[idx],'\t',
          distribution$binom[idx],'\t',
          distribution$pCor[idx],'\n')
    }
    cat('p-val:', p_value,'\t',
        nrow(distribution),'stripes\n',
        'AP/nAP ratio:',proportion,'\n',
        'APs:',sum(distribution$AP),'\t',
        'nAps:',sum(distribution$nAP),'\n')
    
  }
  
}

tempTest <- function(apCounts,
                     pval = 0.05,
                     save = T, 
                     value = NA){
  if(is.na(value)){
    value <- 'All Organisms'
  }
  aps<-apCounts$nPercent[apCounts$isAP == 1]
  aps<-aps[order(aps)]
  naps<-apCounts$nPercent[apCounts$isAP == 0]
  naps<-naps[order(naps)]
  
  la<-rep('AP',length(aps))
  ln<-rep('nAP',length(naps))
  ca<-rep('AP',length(aps))
  cn<-rep('non AP',length(naps))
  
  data <- data.frame(type=c(la,ln),
                     value = c(aps,naps),
                     color = c(ca,cn))
  wt<-wilcox.test(aps, naps,alternative = 't', paired = F)
  # violin ----
  g1<-ggplot(data = data,
         aes(y=value, 
             x=type, 
             #color = color,
             fill = color))+
    theme_bw()+ 
    ggtitle(value)+
    xlab("") +
    ylab("Presence in species") +
    geom_violin(draw_quantiles = c(0.25, 0.75),
                linetype = "dashed",
                scale = 'count',
                trim = F) +
    geom_violin(fill="transparent",
                draw_quantiles = 0.5,
                scale = 'count',
                trim = F) +
    stat_summary(aes(color="mean"),
                 fun=mean, 
                 geom="point", 
                 shape=20, 
                 size=3, 
                 fill="red")+
    annotate(geom = 'label',
             x=c(1.5),
             y=c(0.5),
             label=c(paste0('M-W U test p-val:\n',round(wt$p.value,5))),
             size = c(3),
             hjust = c('center'))+
    
    scale_fill_manual("",guide=F,
                      values = c('AP'=alpha('blue',alpha = 0.25),
                                 'non AP'=alpha('red',alpha = 0.25)))+
    scale_color_manual("",values = c('mean'='red'))
  
  
    
  
  x<-seq(0,length(aps)-1,1)
  x<-(x/length(aps))
  data1<-data.frame(y1=aps,x1=x)
  x<-seq(0,length(naps)-1,1)
  x<-(x/length(naps))
  data2<-data.frame(y2=naps,x2=x)

  
    ggplot()+
    theme_bw()+
    geom_line(data = data2,
              aes(x=x2, y=y2), col='red')+
    geom_line(data = data1,
              aes(x=x1, y=y1), col='blue')
  
    cat('\n####################################\n\n')
    print(ks.test(x=data1$y1,
          y=data2$y2,
          alternative = 'g'))
  cat('\n####################################\n\n')
  print(var.test(aps,naps,alternative = 'g'))
  cat('\n####################################\n\n')
  print(wilcox.test(aps, naps,alternative = 't', paired = F))
  
  p1<-ks.test(data1$y1[data1$y1<.25],
              data2$y2[data2$y2<.25],
              alternative = 'l')$p.value
  p2<-ks.test(data1$y1[data1$y1>=.25&data1$y1<.5],
              data2$y2[data2$y2>=.25&data2$y2<.5],
              alternative = 'l')$p.value
  p3<-ks.test(data1$y1[data1$y1>=.5&data1$y1<.75],
              data2$y2[data2$y2>=.5&data2$y2<.75],
              alternative = 'l')$p.value
  p4<-ks.test(data1$y1[data1$y1>=.75&data1$y1<=1],
              data2$y2[data2$y2>=.75&data2$y2<1],
              alternative = 'l')$p.value
  pLess<-p.adjust(c(p1,p2,p3,p4),method = 'BH')
  
  p1<-ks.test(data1$y1[data1$y1<.25],
              data2$y2[data2$y2<.25],
              alternative = 'g')$p.value
  p2<-ks.test(data1$y1[data1$y1>=.25&data1$y1<.5],
              data2$y2[data2$y2>=.25&data2$y2<.5],
              alternative = 'g')$p.value
  p3<-ks.test(data1$y1[data1$y1>=.5&data1$y1<.75],
              data2$y2[data2$y2>=.5&data2$y2<.75],
              alternative = 'g')$p.value
  p4<-ks.test(data1$y1[data1$y1>=.75&data1$y1<=1],
              data2$y2[data2$y2>=.75&data2$y2<1],
              alternative = 'g')$p.value
  pGreat<-p.adjust(c(p1,p2,p3,p4),method = 'BH')
  plist<-list()
  idx=1
  for(idx in 1:4){
   if(pLess[idx]>pGreat[idx]){
     p<-pGreat[idx]
     if(pGreat[idx]<=pval){
       alt<-'ap>nap'
     }else{
       alt<-'ap=nap'
     }
   }else{
     p<-pLess[idx]
     if(pLess[idx]<=pval){
       alt<-'ap<nap'
     }else{
       alt<-'ap=nap'
     }
   }
    plist[[idx]]<-data.frame(alt=alt,
                             p=p,
                             pLess[idx],
                             pGreat[idx],
                             stringsAsFactors = F)
  }
  
  plist<-do.call(rbind,plist)
  plist$p <-format(plist$p, scientific = T,digits=3)
  
  
  ksG<-ks.test(x=data1$y1,
                y=data2$y2,
                alternative = 'g')$p.value
  
  ksL<-ks.test(x=data1$y1,
               y=data2$y2,
               alternative = 'l')$p.value
  
  if(ksL>ksG){
    ksOA<-ksG
    if(p<=pval){
      alt<-'ap>nap'
    }else{
      alt<-'ap=nap'
    }
  }else{
    ksOA<-ksL
    if(p<=pval){
      alt<-'ap<nap'
    }else{
      alt<-'ap=nap'
    }
  }
  ksOA<-format(ksOA,scientific = T, digits = 3)
  # ks plot ----
  g2<-ggplot()+
    theme_bw()+
    ggtitle(value)+
    xlab("Presence in species") +
    ylab("KS EDCF") +    geom_rect(aes(xmin = 0, xmax = 0.25,
                  ymin = 0, ymax = 1,
                  fill = plist$alt[1]),
              col=alpha("gray",1),lty=3)+
    geom_rect(aes(xmin = 0.25, xmax = 0.5,
                  ymin = 0, ymax = 1,
                  fill = plist$alt[2]),
              col=alpha("gray",1),lty=3)+
    geom_rect(aes(xmin = .5, xmax = 0.75,
                  ymin = 0, ymax = 1,
                  fill = plist$alt[3]),
              col=alpha("gray",1),lty=3)+
    geom_rect(aes(xmin = 0.75, xmax = 1,
                  ymin = 0, ymax = 1,
                  fill = plist$alt[4]),
              col=alpha("gray",1),lty=3)+
    stat_ecdf(data=data1,
              aes(y1, color ='AP'),
              n=50,
              geom = 'step')+
    stat_ecdf(data=data2,
              aes(y2, color ='nAP'),
              n=50,
              geom = 'step')+
    annotate(geom = 'label',
             x=c(0.125,
                 0.375,
                 0.625,
                 0.875,
                 0.5),
             y=c(.95,
                 .95,
                 .95,
                 .95,
                 0.5),
             label=c(paste0(plist$alt[1],'\np-val: ',plist$p[1]),
                     paste0(plist$alt[2],'\np-val: ',plist$p[2]),
                     paste0(plist$alt[3],'\np-val: ',plist$p[3]),
                     paste0(plist$alt[4],'\np-val: ',plist$p[4]),
                     paste0('Over all p-val: ',ksOA,'\n',alt)),
             size = c(3,3,3,3,3),
             hjust = c('center',
                       'center',
                       'center',
                       'center',
                       'center'),
             fill=c(alpha('white',0.5),
                    alpha('white',0.5),
                    alpha('white',0.5),
                    alpha('white',0.5),
                   alpha('green',alpha = 0.5)))+
    scale_fill_manual("",guide=F,
                      values = c('ap>nap'=alpha('blue',alpha = 0.15),
                                 'ap<nap'=alpha('red',alpha = 0.15),
                                 'ap=nap'=alpha('white',0)))+
    scale_color_manual("",values = c('AP' = 'blue',
                                     'nAP'= 'red'))
  g2
  
  plot(g1)
  # save ----
  if(save){
    fileName=file.path(dirFig,paste0(value,'violin.pdf'))
    ggsave(filename = fileName, 
           plot = g1,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
    fileName=file.path(dirFig,paste0(value,'ksTest.pdf'))
    ggsave(filename = fileName, 
           plot = g2,
           device = "pdf",
           width = 11,height = 8,
           dpi=600)
  }
  
  cat('\n####################################\n\n
      Segmentated KS test \n')
  
  cat('Less:',pLess,'\nGreat',pGreat,'\n')
}

# FIM ----

rm(list=ls())

library(ggplot2)
library(gridExtra)

# Import JAGS outputs with posterior samples
out2 <- readRDS(file.path("BBS_example","outputs","out_no-wetland.rds"))
out3 <- readRDS(file.path("BBS_example","outputs","out_no-open.rds"))

# Import data
dat1 <- read.csv(file.path("BBS_example","data","BBS2013_NativBirdRich_LULC.csv"))

# Base covar data for predictions
tmp <- matrix(0,nrow=100,ncol=6)
nomes <- names(dat1)[3:8]
colnames(tmp) <- nomes
xmat <- cbind(1,tmp)

# Get parameters posteriors
# Model 2
betas2 <- cbind(out2$sims.list$beta0,out2$sims.list$betas)
nomes2 <- nomes[nomes!='Wetland']
colnames(betas2) <- c('interc',nomes2)
# Model 3
betas3 <- cbind(out3$sims.list$beta0,out3$sims.list$betas)
nomes3 <- nomes[nomes!='Open']
colnames(betas3) <- c('interc',nomes3)

#which betas are significant?
# Model 2
signif2 <- apply(betas2,2,quantile,c(0.025,0.975))
cores2 <- rep('grey',ncol(signif2))
cores2[signif2[1,]>0 & signif2[2,]>0] <- 'blue'
cores2[signif2[1,]<0 & signif2[2,]<0] <- 'red'
cores2 <-data.frame(cores2=cores2,nomes=colnames(betas2))
# Model 3
signif3 <- apply(betas3,2,quantile,c(0.025,0.975))
cores3 <- rep('grey',ncol(signif3))
cores3[signif3[1,]>0 & signif3[2,]>0] <- 'blue'
cores3[signif3[1,]<0 & signif3[2,]<0] <- 'red'
cores3 <- data.frame(cores3=cores3,nomes=colnames(betas3))

# Conditional plot for model 2 --------------------------------------------
# Left-out Wetland
covar <- seq(from=0,to=1,length.out=100)
graph2 <- list()
for(i in 1:length(nomes2)){
  ind=which(colnames(xmat)=='Wetland'); xmat1=xmat[,-ind]
  xmat1[,nomes2[i]] = covar
  media=exp(xmat1%*%t(betas2))
  resumo=apply(media,1,quantile,c(0.025,0.5,0.975))
  resumo1=data.frame(est=resumo[2,],
                     lcl=resumo[1,],
                     ucl=resumo[3,])
  cores2a=cores2$cores2[cores2$nomes==nomes2[i]]
  
  graph2[[i]] <- ggplot(data=resumo1,
                  aes(x=covar*100,y=est,ymin=lcl,ymax=ucl,color='denis')) +
    labs(x=paste(nomes2[i],"%"),y="Bird Richness",
         title=paste0(letters[i],')')) +
    ylim(40,95) +
    scale_x_continuous(sec.axis=sec_axis(trans=~.*(-1)+100,name='Wetland %')) +
    geom_line(linewidth=1) +
    scale_color_manual(values=cores2a) +
    geom_ribbon(alpha=.4, fill = "grey80") +
    theme_classic(base_size=20) +
    theme(axis.line.x.top=element_line(color="gray50"),
          axis.ticks.x.top=element_line(color="gray50"),
          axis.text.x.top=element_text(color="gray50"),
          axis.title.x.top=element_text(color="gray50"),
          legend.position="none")
  # nomes2a=paste0('mod2 ',nomes2[i],'.jpg')
  # ggsave(nomes2a,plot=plot2,width=16,height=16,unit="cm",dpi=300)
}

# Conditional plot for model 3 --------------------------------------------
# Left-out Open
graph3 <- list()
for(i in 1:length(nomes3)){
  ind=which(colnames(xmat)=='Open'); xmat1=xmat[,-ind]
  xmat1[,nomes3[i]] = covar
  media=exp(xmat1%*%t(betas3))
  resumo=apply(media,1,quantile,c(0.025,0.5,0.975))
  resumo1=data.frame(est=resumo[2,],
                     lcl=resumo[1,],
                     ucl=resumo[3,])
  cores3a=cores3$cores3[cores3$nomes==nomes3[i]]
  
  graph3[[i]] <- ggplot(data=resumo1,
                  aes(x=covar*100,y=est,ymin=lcl,ymax=ucl,color='denis')) +
    labs(x=paste(nomes3[i],"%"),y="",
         title=paste0(letters[i+4],')')) +
    ylim(40,95) +
    scale_x_continuous(sec.axis=sec_axis(trans=~.*(-1)+100,name='Open Habitat %')) +
    geom_line(linewidth=1) + 
    scale_color_manual(values=cores3a) +
    geom_ribbon(alpha=.4,fill='grey80') +
    theme_classic(base_size=20) +
    theme(axis.line.x.top=element_line(color="gray50"),
          axis.ticks.x.top=element_line(color="gray50"),
          axis.text.x.top=element_text(color="gray50"),
          axis.title.x.top=element_text(color="gray50"),
          legend.position='none')
  # nomes3a=paste0('mod3 ',nomes3[i],'.jpg')
  # ggsave(nomes3a,plot=plot3,width=16,height=16,unit="cm",dpi=300)
}

#combine everything and export
fig.condPlots <- grid.arrange(graph2[[1]], graph3[[1]],
                              graph2[[2]], graph3[[2]],
                              graph2[[3]], graph3[[3]],
                              graph2[[4]], graph3[[4]],
                              ncol=2)

ggsave(file.path("BBS_example","conditional_plots.png"),fig.condPlots,
       width=33,height=44,unit="cm",dpi=300)



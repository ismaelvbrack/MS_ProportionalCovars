rm(list=ls())

library(jagsUI)
library(ggplot2)
library(gridExtra)
library(gtools)

# Import JAGS outputs with posterior samples
out2 <- readRDS(file.path("BBS_example","outputs","out_no-wetland.rds"))
out3 <- readRDS(file.path("BBS_example","outputs","out_no-open.rds"))

# Import data
dat1 <- read.csv(file.path("BBS_example","data","BBS2013_NativBirdRich_LULC.csv"))
nomes <- names(dat1)[3:8]

#get parameters
betas2 <- cbind(out2$sims.list$beta0,out2$sims.list$betas)
nomes2 <- nomes[nomes!='Wetland']
colnames(betas2) <- c('interc',nomes2)

betas3 <- cbind(out3$sims.list$beta0,out3$sims.list$betas)
nomes3 <- nomes[nomes!='Open']
colnames(betas3) <- c('interc',nomes3)

# Randomly draw proportional covariates
# seq1=seq(from=0,to=1,length.out=10)
# xmat <- expand.grid(x1=seq1,
#                     x2=seq1,
#                     x3=seq1,
#                     x4=seq1,
#                     x5=seq1,
#                     x6=seq1)
# soma=apply(xmat,1,sum)
# xmat=xmat[soma==1,]; dim(xmat)
xmat0 <- rdirichlet(2000,alpha=rep(0.1,6))
xmat1 <- rdirichlet(2000,alpha=rep(1,6))
xmat <- rbind(xmat0,xmat1)
colnames(xmat) <- nomes

#make predictions
# Model 2
xmat2 <- cbind(1,data.matrix(xmat[,nomes2]))
pred2 <- exp(xmat2%*%t(betas2))
# Model 3
xmat3 <- cbind(1,data.matrix(xmat[,nomes3]))
pred3 <- exp(xmat3%*%t(betas3))

#combine everything
pred.fim <- as.data.frame(xmat)
pred.fim$pred2 <- apply(pred2,1,median)
pred.fim$pred3 <- apply(pred3,1,median)

# Predictions based on model 2 and 3 are equivalent
plot(pred3~pred2,data=pred.fim)
rango <- c(0,1000)
lines(rango,rango,col='red')
range(pred.fim$pred2-pred.fim$pred3)

# Marginal plots ----------------------------------------------------------
margPlots <- list()
for(i in 1:length(nomes)){
  pred.fim$covar=pred.fim[,nomes[i]]
  margPlots[[i]] <- ggplot(data=pred.fim,
                           aes(x=covar*100,y=pred2)) +
    geom_point(shape=1) + 
    geom_smooth(method="glm",method.args=list(family=gaussian(link='log')),se=FALSE, color="red") +
    labs(x=paste(nomes[i],"%"),y="Bird Richness") +
    ylim(50,85) +
    theme_classic(base_size=12)
}

fig.margPlots <- grid.arrange(margPlots[[1]],margPlots[[2]],margPlots[[3]],
                              margPlots[[4]],margPlots[[5]],margPlots[[6]],
                  ncol=3)


ggsave(file.path("BBS_example","BBSex_margPlots.png"),fig.margPlots,
       width=28,height=14,unit="cm",dpi=300)






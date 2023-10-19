
library(jagsUI)

# Importing data ----------------------------------------------------------

# Import richness estimates per route
R2013 <- read.csv(file.path("BBS_example","data","BBS_NativeBirds_Richness2013.csv"))

# Import LULC proportions per route
rte.lulc <- read.csv(file.path("BBS_example","data","LULC20km_BBS-routes_2013.csv"))

# NA to 0
rte.lulc[,2:ncol(rte.lulc)] <- apply(rte.lulc[,2:ncol(rte.lulc)], 2, function(x) ifelse(is.na(x), 0, x))

any(!R2013$rteno %in% rte.lulc$rteno) # FALSE

# Merge
dat1 <- merge(R2013, rte.lulc, by="rteno")
#write.csv(dat1,file.path("BBS_example","data","BBS2013_NativBirdRich_LULC.csv"),row.names=F)

# Model 1: Full model with 6 covars --------------------------------------------------
data1 <- list(R=dat1$est_species_2013, covars=dat1[,3:8])

# Initial values
inits <- function() list(beta0=rnorm(1),
                         betas=rnorm(6),
                         r=runif(1,1,10))

# Parameters monitored
params <- c("beta0","betas","r")

# MCMC settings
ni <- 12000
nt <- 1
nb <- 4000
nc <- 3
na <- 1000

# Fitting
out1 <-  jags(data1, inits, params,
              model.file=file.path("BBS_example","JAGS_models",'full-model_Bayes.R'),
              n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

# Model 2: drop wetlands --------------------------------------------------
data2 <- list(R=dat1$est_species_2013, covars=dat1[,3:7])

# Initial values
inits <- function() list(beta0=rnorm(1),
                         betas=rnorm(5),
                         r=runif(1,1,10))

# Parameters monitored
params <- c("beta0","betas","r","lambda")

# MCMC settings
ni <- 12000
nt <- 1
nb <- 4000
nc <- 3
na <- 1000

# Fitting
out2 <-  jags(data2, inits, params,
              model.file=file.path("BBS_example","JAGS_models",'wo1covar-model_Bayes.R'),
              n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

# Model 3: drop Open vegetation --------------------------------------------------
data3 <- list(R=dat1$est_species_2013, covars=dat1[,c(3:5,7:8)])

# Initial values
inits <- function() list(beta0=rnorm(1),
                         betas=rnorm(5),
                         r=runif(1,1,10))

# Parameters monitored
params <- c("beta0","betas","r","lambda")

# MCMC settings
ni <- 12000
nt <- 1
nb <- 4000
nc <- 3
na <- 1000

# Fitting
out3 <-  jags(data3, inits, params,
              model.file=file.path("BBS_example","JAGS_models",'wo1covar-model_Bayes.R'),
              n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

# Model 4: without intercept --------------------------------------------------
data1

# Initial values
inits <- function() list(betas=rnorm(6),
                         r=runif(1,1,10))

# Parameters monitored
params <- c("betas","r")

# MCMC settings
ni <- 12000
nt <- 1
nb <- 4000
nc <- 3
na <- 1000

# Fitting
out4 <-  jags(data1, inits, params,
              model.file=file.path("BBS_example","JAGS_models",'woInter-model_Bayes.R'),
              n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

#* Save results
saveRDS(out1,file.path("BBS_example","outputs","out_full-model.rds"))
saveRDS(out2,file.path("BBS_example","outputs","out_no-wetland.rds"))
saveRDS(out3,file.path("BBS_example","outputs","out_no-open.rds"))
saveRDS(out4,file.path("BBS_example","outputs","out_no-intercept.rds"))


# Figure coefficients -----------------------------------------------------
out1 <- readRDS(file.path("BBS_example","outputs","out_full-model.rds"))
out2 <- readRDS(file.path("BBS_example","outputs","out_no-wetland.rds"))
out3 <- readRDS(file.path("BBS_example","outputs","out_no-open.rds"))
out4 <- readRDS(file.path("BBS_example","outputs","out_no-intercept.rds"))

library(ggplot2)
library(ggstance)

coefs <- data.frame(par=rep(names(dat1)[3:8],3),
           mean=c(out4$mean$betas, out2$mean$betas,NA,   out3$mean$betas[1:3],NA,out3$mean$betas[4:5]),
           lcl=c(out4$q2.5$betas,  out2$q2.5$betas,NA,   out3$q2.5$betas[1:3],NA,out3$q2.5$betas[4:5]),
           ucl=c(out4$q97.5$betas,  out2$q97.5$betas,NA,  out3$q97.5$betas[1:3],NA,out3$q97.5$betas[4:5]),
           model=factor(rep(c("w/o Intercept","w/o Wetland","w/o Open"),each=6))
           )

# Compare with a MLE Poisson regression
cbind(Bayes_NegBin=coefs[which(coefs$model=="w/o Intercept"),"mean"],
      MLE_Pois=coef(glm(est_species_2013~Crop+Developed+Forest+Open+Water+Wetland-1, data=dat1, family="poisson"))
)


# Figure was ugly when including...
coefs <- coefs[-which(coefs$model=="w/o Intercept"),]

ggplot(data=coefs,aes(x=par,y=mean,ymin=lcl,ymax=ucl,color=model)) +
  geom_point(size=3.5,position=position_dodge(.3)) + 
  geom_errorbar(size=1.2,width=.3,position=position_dodge(.3)) + 
  scale_color_manual(values=c("dodgerblue3","darkorange2")) +
  geom_hline(yintercept=0,linetype=2,col="gray",size=.8) +
  theme_classic(base_size=14) +
  labs(y="Coefficient estimate",x="Variable") +
  theme(legend.position="top") +
  coord_flip() 







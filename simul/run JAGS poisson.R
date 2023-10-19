rm(list=ls(all=T))
library(jagsUI)
set.seed(1)


dat1=read.csv(file.path("simul",'fake data.csv'))
nobs=nrow(dat1)

# MCMC settings 
ni <- 15000 #number of iterations
nt <- 50    #interval to thin 
nb <- 5000  #number of iterations to discard as burn-in
nc <- 3     #number of chains

#set parameters to monitor
params=c("b0","b1","b2",'b3')

#create function that sets the initial values
inits=function(){
  list(b0=rnorm(1,0,1), 
       b1=rnorm(1,0,1), 
       b2=rnorm(1,0,1),
       b3=rnorm(1,0,1))
}

#run model
mod=jags(model.file=file.path("simul","JAGS poisson regression.R"),
               parameters.to.save=params,inits=inits,
               data=list(nobs=nobs,
                         rich=dat1$rich,
                         pfor=dat1$pfor,
                         pagri=dat1$pagri,
                         pwet=dat1$pwet),
               n.chains=nc,
               n.burnin=nb,n.iter=ni,n.thin=nt,DIC=TRUE)
summary(mod)
mod

param=c(mod$mean$b0,mod$mean$b1,mod$mean$b2,mod$mean$b3)
delta=3-param[1]; delta
param[2:4]-delta

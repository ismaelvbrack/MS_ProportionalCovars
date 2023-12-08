rm(list=ls())
library('gtools')
library(meifly)
set.seed(1)

source('useful functions.R')

#model selection using AIC
nsim=1000
res=matrix(NA,nsim,4)
for (i in 1:nsim){
  print(i)
  dat1=generate.data()  
  tmp=aic.mod.sel(dat1=dat1)
  res[i,1:length(tmp)]=tmp
}

#summarize results
table(res[,1]) #intercept is always present
table(res[,4]) #cov3 is always missing. This implies always only 2 covariates
res1=data.frame(cov1=res[,2],
                cov2=res[,3],
                denis=1)
res2=aggregate(denis~cov1+cov2,data=res1,sum)

#export results
write.csv(res2,'model selection AIC.csv',row.names=F)

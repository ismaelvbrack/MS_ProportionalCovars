rm(list=ls())
library('gtools')
set.seed(1)

#settings
n=1000
# intercept
b0=3 
# slopes
b1=-1
b2=0
b3=0.5

#covariates
covs=rdirichlet(n,rep(1,3)) # create random proportional covariate data
colnames(covs)=c('pfor','pagri','pwet')

hist(covs[,'pfor']) # histogram

cor(covs) # correlations

#response
rich=rpois(n,exp(b0+b1*covs[,'pfor']+b2*covs[,'pagri']+b3*covs[,'pwet'])) # create richness variable depending on covs
sort(unique(rich))

#combine everything and export
dat1=data.frame(rich=rich,
                pfor=covs[,'pfor'],
                pagri=covs[,'pagri'],
                pwet=covs[,'pwet'],
                region=sample(1:10,n,replace=T),
                x=runif(n),
                y=runif(n))


write.csv(dat1,file.path("simul",'fake_data.csv'),row.names=F)

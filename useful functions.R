#generate data with dirichlet covariates
generate.data=function(){
  #settings
  n=1000
  b0=3
  b1=-1
  b2=0
  b3=0.5
  
  #covariates
  covs=rdirichlet(n,rep(1,3))
  colnames(covs)=c('pfor','pagri','pwet')

  #response
  rich=rpois(n,exp(b0+b1*covs[,'pfor']+b2*covs[,'pagri']+b3*covs[,'pwet']))
  sort(unique(rich))
  
  #combine everything and export
  dat1=data.frame(rich=rich,
                  pfor=covs[,'pfor'],
                  pagri=covs[,'pagri'],
                  pwet=covs[,'pwet'],
                  region=sample(1:10,n,replace=T),
                  x=runif(n),
                  y=runif(n))
  dat1
}
#----------------------
#AIC model selection
aic.mod.sel=function(dat1){
  x = dat1[,c('pfor','pagri','pwet')]
  y = dat1$rich
  
  #run all subsets using the "fitall" function
  fit_all_mods <- fitall(y, x, "glm")
  
  ## Extract AIC from each model
  fitall.out.aic <- t(sapply(fit_all_mods, extractAIC))
  
  ## Show the result for the best model
  final.out.order <-order(fitall.out.aic[,2])
  models<-fit_all_mods[final.out.order][1]
  coef1=models[[1]]$coefficients
  names(coef1)
}
#--------------------------
#generate data with regular covariates
generate.data.reg.cov=function(){
  #settings
  n=1000
  b0=3
  b1=-1
  b2=0
  b3=0.5
  
  #covariates
  covs=matrix(rnorm(n*3),n,3)
  colnames(covs)=c('pfor','pagri','pwet')
  
  #response
  rich=rpois(n,exp(b0+b1*covs[,'pfor']+b2*covs[,'pagri']+b3*covs[,'pwet']))
  sort(unique(rich))
  
  #combine everything and export
  dat1=data.frame(rich=rich,
                  pfor=covs[,'pfor'],
                  pagri=covs[,'pagri'],
                  pwet=covs[,'pwet'],
                  region=sample(1:10,n,replace=T),
                  x=runif(n),
                  y=runif(n))
  dat1
}
#----------------------
#BIC model selection
bic.mod.sel=function(dat1){
  x = dat1[,c('pfor','pagri','pwet')]
  y = dat1$rich
  
  #run all subsets using the "fitall" function
  fit_all_mods <- fitall(y, x, "glm")
  
  ## Extract AIC from each model
  fitall.out.bic <- t(sapply(fit_all_mods, BIC))
  
  ## Show the result for the best model
  final.out.order <-order(fitall.out.bic)
  models<-fit_all_mods[[final.out.order[1]]]
  coef1=models$coefficients
  names(coef1)
}

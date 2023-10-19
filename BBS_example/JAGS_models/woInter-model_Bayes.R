
model{
  
  for(k in 1:6){
    betas[k] ~ dnorm(0,1/1000)
  }
  
  # Negative binomial dipersion par
  r ~ dunif(0,50)
  
  # Likelihood
  for(i in 1:length(R)){
    # Richness
    R[i] ~ dnegbin(probs[i],r)
    
    # transform 1st parameter from the mean/variance
    probs[i] <- r/(r+lambda[i])
    
    # linear relationship using the mean
    log(lambda[i]) <- betas[1]*covars[i,1] + betas[2]*covars[i,2] + 
      betas[3]*covars[i,3] + betas[4]*covars[i,4] + betas[5]*covars[i,5] + 
      betas[6]*covars[i,6] 
  } #i
  
}
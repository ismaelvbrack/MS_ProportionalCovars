model{
  for (i in 1:nobs){
    rich[i] ~ dpois(exp(b0+b1*pfor[i]+b2*pagri[i]+b3*pwet[i]))
  }
  b0 ~ dnorm(0,0.1)
  b1 ~ dnorm(0,0.1)
  b2 ~ dnorm(0,0.1)
  b3 ~ dnorm(0,0.1)
}
rm(list=ls())
set.seed(1)
library('lme4')
library('mgcv')

# Load data
dat1=read.csv('fake data.csv')



# Log-normal LM -----------------------------------------------------------
mod.lm=lm(log(rich)~pfor+pagri+pwet,data=dat1); summary(mod.lm)
delta=3-mod.lm$coefficients[1]; delta
mod.lm$coefficients[2:4]-delta


# Poisson GLM -------------------------------------------------------------
mod.glm=glm(rich~pfor+pagri+pwet,data=dat1,family=poisson); summary(mod.glm)
delta=3-mod.glm$coefficients[1]; delta
mod.glm$coefficients[2:4]-delta


# Poisson GLMM with region random effects --------------------------------
mod.glmm=glmer(rich~pfor+pagri+pwet+(1|region),data=dat1,family=poisson); summary(mod.glmm)
delta=3-mod.glmm@beta[1]; delta
mod.glmm@beta[2:4]-delta


# Poisson GAM with spatial autocorrelation --------------------------------
mod.gam=gam(rich~pfor+pagri+pwet+s(x, y, bs="tp"),data=dat1,family=poisson); summary(mod.gam)
delta=3-mod.gam$coefficients[1]; delta
mod.gam$coefficients[2:4]-delta

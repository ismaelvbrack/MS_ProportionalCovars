rm(list=ls())
library('gtools')
library('ggplot2')
set.seed(1)

#basic settings
n=1000 #number of observations
ncov=3 #number of covariates

#parameters
b0=3
b1=-1
b2=0
b3=0.5

#covariates
covs=rdirichlet(n,rep(1,ncov))
colnames(covs)=c('pfor','pagri','pwet')

#response
rich=rpois(n,exp(b0+b1*covs[,'pfor']+b2*covs[,'pagri']+b3*covs[,'pwet']))

#combine everything
dat1=data.frame(rich=rich,
                pfor=covs[,'pfor'],
                pagri=covs[,'pagri'],
                pwet=covs[,'pwet'])

#fit model
mod1=glm(rich~pfor+pagri,data=dat1,family='poisson')
coef1=coef(mod1)

#create conditional plots in base R
pfor=seq(from=0,to=1,length.out=1000) 
mean1=exp(coef1['(Intercept)']+coef1['pfor']*pfor+coef1['pagri']*0) 
plot(pfor,mean1,type='l',xlab="pfor",ylab='Species richness (R)')

#add second x-axis
z=seq(from=0,to=1,by=0.2)
axis(side=3,at=z,labels=z[length(z):1],cex.axis=1)
mtext(side=3,line=3,'pwet',cex=1)

#create conditional plots using ggplot
dat1=data.frame(pfor=pfor,
                mean.spp.richness=mean1,
                pwet=1-pfor)
ggplot(dat1,aes(x=pfor,y=mean.spp.richness))+
  geom_line(color='black')+
  scale_x_continuous(name='pfor',
                     sec.axis=sec_axis(trans=~.*(-1)+1,name='pwet'))+
  ylab('Species Richness (R)')

#randomly drawn proportion covariates
n=1000
covs=rdirichlet(n,rep(1,ncov))
colnames(covs)=c('pfor','pagri','pwet')

#calculate the mean
mean1=exp(coef1['(Intercept)']+coef1['pfor']*covs[,'pfor']+coef1['pagri']*covs[,'pagri'])

#draw polygon 
pfor=seq(from=0,to=1,length.out=100)
pagri=1-pfor
#upper line (increase pfor, decrease pwet)
upper1=exp(coef1['(Intercept)']+coef1['pfor']*pfor) 
#lower line (increase pfor, decrease pagri)
lower1=exp(coef1['(Intercept)']+coef1['pfor']*pfor+coef1['pagri']*pagri) 

#create marginal plot using base R
plot(covs[,'pfor'],mean1,xlab="pfor",ylab='Species richness (R)')
coord=data.frame(x=c(pfor,pfor[length(pfor):1]),
                 y=c(upper1,lower1[length(pfor):1]))
polygon(coord$x,coord$y,col='grey')
points(covs[,'pfor'],mean1)

#create marginal plots in ggplot2
dat1=data.frame(pfor=covs[,'pfor'],
                pagri=covs[,'pagri'],
                pwet=covs[,'pwet'],
                mean.spp.richness=mean1)
ggplot()+
  geom_polygon(data=coord,aes(x=x,y=y),fill='grey')+
  geom_point(data=dat1,aes(x=pfor,y=mean.spp.richness),size=2,shape=1)+
  theme_bw()+
  xlab('pfor')+
  ylab('Species richness (R)')

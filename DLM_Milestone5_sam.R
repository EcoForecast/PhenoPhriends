

#dat.test = dat%>%select(contains('BART'))
library(rjags)
library(ecoforecastR)

BART <- dat[ which(dat$siteID== "BART"), ]
                         
time <- as.Date(BART$time)
y = BART$gcc_90
data <- list(y=log(y),n=length(y),x_ic=log(1000),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)  #took out y=log(y)
plot(time,y)


#The model...
ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed="~ 1 + X"),data)
names(ef.out)

summary(ef.out$params)


time.rng=c(1,length(time))
out <- as.matrix(ef.out$predict)
ci <- apply(exp(out),2,quantile,c(0.025,0.5,0.975))
plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="GCC",log='y',xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)


strsplit(ef.out$model,"\n",fixed = TRUE)[[1]]


PhenoDLM = " 
  model{
  #### Data Model
  for(t in 1:n){
  OBS[t] ~ dnorm(x[t],tau_obs)
  Xf[t,1] ~ dnorm(muXf[1],tauXf[1])
  }
  
 #### Process Model
 for(t in 2:n){
 mu[t] <- x[t-1]  + betaX*x[t-1] + betaIntercept*Xf[t,1]
 x[t]~dnorm(mu[t],tau_add)
 }


 #### Priors
 x[1] ~ dnorm(x_ic,tau_ic)
 tau_obs ~ dgamma(a_obs,r_obs)
 tau_add ~ dgamma(a_add,r_add)
 
  #### Random Effects
  #RANDOM  tau_alpha~dgamma(0.1,0.1)
  #RANDOM  for(i in 1:nrep){
  #RANDOM   alpha[i]~dnorm(0,tau_alpha)
  }
  
   #### Fixed Effects
    betaX ~dnorm(0,0.001)
    betaIntercept~dnorm(0,0.001)
    for(j in 1: 1 ){
    muXf[j] ~ dnorm(0,0.001)
    tauXf[j] ~ dgamma(0.01,0.01)
    
    }
}
"




















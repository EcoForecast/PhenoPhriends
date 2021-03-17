

#dat.test = dat%>%select(contains('BART'))
library(rjags)
library(ecoforecastR)

#let's try just using the BART data before we run it all
#BART <- dat[ which(dat$siteID== "BART"), ]
                   
##generating a time series:      
time <- as.Date(dat$time)
y <- dat$gcc_90
#plot to test time series
plot(time,y)

#define data and priors as list
data <- list(y=y,n=length(y),x_ic=mean(na.omit(y)),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)
data.test<-list(n=length(y),x_ic=mean(na.omit(y)),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)

#running the dynamic linear model for BART
ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed="~ 1 + X"),data)
names(ef.out)

summary(ef.out$params)

#plot time series with confidence intervals
time.rng=c(1,length(time))
out <- as.matrix(ef.out$predict)
ci <- apply((out),2,quantile,c(0.025,0.5,0.975))
plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="GCC",log='y',xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)



#DOING AN MCMC

#defining initial state of model parameters
nchain <- 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(na.omit(y),length(y),replace=TRUE) 
  #init[[i]] <- list(tau_add=1/var(diff(y.samp)),tau_obs=5/var(y.samp)) 
  init[[i]] <- list (tau_add=1/var(diff(y.samp)),tau_obs=1/var(y.samp))
}
## ISSUES IN CODE BLOCK ABOVE^^^
#about 5/var(y.samp)< was left over from the exercise 6 code, not sure if it is accurate for our purposes
#had to use (na.omit(y)) in y.samp call. Otherwise my init list contained only NA's.


#print the model fit under the hood
#strsplit(ef.out$model,"\n",fixed = TRUE)[[1]]
pheno.model<-as.character(strsplit(ef.out$model,"\n",fixed = TRUE)[[1]])

j.pheno.model.test <- jags.model (file = textConnection(pheno.model),
                             data = data.test,
                             inits = init,
                             n.chains = 3)


j.pheno.out <- coda.samples (model = j.pheno.model.test,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 5000)    #FOR ACTUAL RUN LETS CHANGE IT TO 5000
plot(j.pheno.out)

file.name <- paste0('MCMC_Plots/MCMC',(Sys.Date()-1),'.jpeg')
jpeg(file=file.name)
plot(j.pheno.out)
dev.off()








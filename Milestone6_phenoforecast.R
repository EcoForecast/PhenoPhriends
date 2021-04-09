
#making a forecast and having fun

##HERE IS WHAT GOES IN:
#IC=initial conditions, from j.pheno.out$params, see below
#tempcast=max temp forecast from NOAA ensembles
#beta=slope of temp data (assessed from daymet data?)
#q=process error tau_add
#Nmc=# of mcmc runs
#gmin=default value min gcc
#gmax=default value max gcc

#the timestep is 16 days:
NT=16
#the number of ensemble members is 10:
Nmc=10


#we set gcc min and max values, they are different for each run/site and they are here:
gmin=data$gmin
gmax=data$gmax


#FORECAST FUNCTION
phenoforecast <- function(IC,tempcast,beta,Q,n=Nmc,gmin,gmax){
  N <- matrix(NA,n,NT)  
  Nprev <- IC
  for(t in 1:NT){
    mu =  Nprev + beta*tempcast[t,] #or [,t] depending on dim
    N[,t] <- pmax(pmin(rnorm(n,mu,Q),gmax),gmin) #ensuring we are btw min and max we set                        
    Nprev <- N[,t]                                
  }
  return(N)
}


#finding mean temp from NOAA ensembles

#WAIT! do unit conversions first because its in Kelvin!
#make function to convert from kelvin to celsius (like daymet data we used to calibrate the model)
k.to.c<-function(k){
  return(k-273.15)
}

#noaa temp data in celsius
df1.c<-apply(df1,2,k.to.c)

#now we need to group them by site
df1.BART<-df1.c[1:31,]
df1.CLBJ<-df1.c[32:62,]
df1.DELA<-df1.c[63:93,]
df1.GRSM<-df1.c[94:124,]
df1.HARV<-df1.c[125:155,]
df1.SCBI<-df1.c[156:186,]
df1.STEI<-df1.c[187:217,]
df1.UKFS<-df1.c[218:248,]

#findmaxtemp<-function(x){
 # return(max(x))
#}
#BART.temp.test<-tapply(df1.BART,day,max)

findmaxtemp<-function(x){
  try=as.vector(x)
  return(tapply(try, rep(1:16, each=24), max))
}

#IN OFFICE HOURS WE ENDED UP USING DF1.BART, NOT THESE:
# temp.max<-matrix(NA,16,8)
# temp.max[,1]<-findmaxtemp(df1.BART)
# temp.max[,2]<-findmaxtemp(cel.CLBJ)
# temp.max[,3]<-findmaxtemp(cel.DELA)
# temp.max[,4]<-findmaxtemp(cel.GRSM)
# temp.max[,5]<-findmaxtemp(cel.HARV)
# temp.max[,6]<-findmaxtemp(cel.SCBI)
# temp.max[,7]<-findmaxtemp(cel.STEI)
# temp.max[,8]<-findmaxtemp(cel.UKFS)
# colnames(temp.max)=siteID
# 
# 
# 

#MUST DO FOR ALL SITES
temp.max <- matrix(findmaxtemp(df1.BART[1,-1]),ncol=1)  #drops the 1st observation (analysis)

temp.max <- apply(df1.BART[,-1],1,findmaxtemp) #days vs ensemble members
temp.max.mean<-matrix(apply(temp.max,1,mean),ncol=1)

## parameters
params <- as.matrix(j.pheno.out)
param.mean <- apply(params,2,mean)
beta<-param.mean["betaTemp"]
q<-1/sqrt(param.mean["tau_add"])
## initial conditions
IC <-data$mu_ic  ##we don't have this? START @ END OF GCC TIME SERIES AND ITS UNCERTAINTY(sd) FOR EACH SITE
  
phiend<-phenoforecast(IC,temp.max,beta,q,Nmc,gmin,gmax)
#next steps: compute confidence intervals, add in uncertainties 1 by one, do for 35 not 16, then set up for all sites,THEN assess where we're at

time=1:NT


#---------------trying the deterministic---------
#doing for BART--this does not work!
PhF.BART<-phenoforecast(IC=IC,
                      tempcast=temp.max.mean,
                      beta=param.mean["betaTemp"],
                      Q=0,
                      n=Nmc,
                      gmin=gmin,
                      gmax=gmax)



plot(0,0, xlim=c(0,NT),ylim=range(Ph.det.BART))
for (p in 1:Nmc){
  points(PhF.BART[p,],type="l",col=p)
}

#this will make confidence intervals
time.f<-1:NT
ci <- apply(as.matrix(PhF.BART.IP),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART))
ecoforecastR::ciEnvelope(time.f,ci[1,],ci[3,],col=col.alpha("lightBlue",0.6))

#-----------------
#initial condition ensemble created from last gcc observation point & sd
IC.ens<-rnorm(Nmc,tail(BART$gcc_90,1),tail(BART$gcc_sd,1))
PhF.BART.IC<-phenoforecast(IC=IC.ens,
                        tempcast=temp.max.mean,
                        beta=param.mean["betaTemp"],
                        Q=0,
                        n=Nmc,
                        gmin=gmin,
                        gmax=gmax)

time.f<-1:NT
ci <- apply(as.matrix(PhF.BART.IC),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART))
ecoforecastR::ciEnvelope(time.f,ci[1,],ci[3,],col=col.alpha("lightBlue",0.6))

#-----------------
#parameter uncertainty for beta
prow <- sample.int(nrow(params),Nmc,replace=TRUE)
PhF.BART.IP<-phenoforecast(IC=IC.ens,
                           tempcast=temp.max.mean,
                           beta=params[prow,"betaTemp"],
                           Q=0,
                           n=Nmc,
                           gmin=gmin,
                           gmax=gmax)

ci <- apply(as.matrix(PhF.BART.IP),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART))
ecoforecastR::ciEnvelope(time.f,ci[1,],ci[3,],col=col.alpha("lightBlue",0.6))


#---------------
drow<-sample.int(nrow(df1.BART),Nmc,replace=TRUE)
PhF.BART.IPT<-phenoforecast(IC=IC.ens,
                           tempcast=df1.BART[drow,],  #this is not working
                           beta=params[prow,"betaTemp"],
                           Q=0,
                           n=Nmc,
                           gmin=gmin,
                           gmax=gmax)


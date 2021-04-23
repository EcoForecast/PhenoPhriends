library(ecoforecastR)

#making a forecast and having fun

##HERE IS WHAT GOES IN:
#IC=initial conditions, from j.pheno.out$params, see below
#tempcast=max temp forecast from NOAA ensembles
#beta=slope of temp data (assessed from daymet data?)
#q=process error tau_add
#Nmc=# of mcmc runs
#gmin=default value min gcc
#gmax=default value max gcc
##STILL NEED TO SAVE DATA VALUES EACH TIME TO GET GMIN AND GMAX FOR EACH SITE

#the timestep is 16 days:
NT=16
#the number of ensemble members is 10:
Nmc=1000


# #we set gcc min and max values, they are different for each run/site and they are here:
# load(file=paste0(as.character(siteID[i]),".data.Rdata"))
# gmin=data$gmin
# gmax=data$gmax

#load gcc data into list:
site.gcc<-list()
  site.gcc$BART<-BART
  site.gcc$CLBJ<-CLBJ
  site.gcc$DELA<-DELA
  site.gcc$GRSM<-GRSM
  site.gcc$HARV<-HARV
  site.gcc$SCBI<-SCBI
  site.gcc$STEI<-STEI
  site.gcc$UKFS<-UKFS

#getting all IC's for each site:
IC.ens<-list()
for (s in siteID){
   IC.ens[[s]]<-rnorm(Nmc,tail(site.gcc[[s]]$gcc_90,1),tail(site.gcc[[s]]$gcc_sd,1))
}

  


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
#df1.c<-apply(df1,2,k.to.c)
df1.c <- lapply(df1,k.to.c)

###now we need to group them by site
# df1.BART<-df1.c[1:31,]
# df1.CLBJ<-df1.c[32:62,]
# df1.DELA<-df1.c[63:93,]
# df1.GRSM<-df1.c[94:124,]
# df1.HARV<-df1.c[125:155,]
# df1.SCBI<-df1.c[156:186,]
# df1.STEI<-df1.c[187:217,]
# df1.UKFS<-df1.c[218:248,]

#findmaxtemp<-function(x){
# return(max(x))
#}
#BART.temp.test<-tapply(df1.BART,day,max)

findmaxtemp<-function(x){
  try=as.vector(x)
  return(tapply(try, rep(1:16, each=24), max))
}


#MUST DO FOR ALL SITES

#temp.max <- matrix(findmaxtemp(df1.BART[1,-1]),ncol=1)  #drops the 1st observation (analysis)

#temp.max <- apply(df1.c$BART[,-1],1,findmaxtemp) #days vs ensemble members
#temp.max.mean<-matrix(apply(temp.max,1,mean),ncol=1)

#FINDS MAX TEMP ENSEMBLE MEAN FOR EACH SITE:
#temp.max.mean<-list()
#for (s in siteID){
#  temp.max<-apply(df1.c[[s]][,-1],1,findmaxtemp)
#  temp.max.mean[[s]]<-matrix(apply(temp.max,1,mean),ncol=1)
#}

temp.max<-list()
temp.max.mean<-list()
for (s in siteID){
  temp.max[[s]]<-matrix(apply(df1.c[[s]][,-1],1,findmaxtemp),nrow=NT)
  temp.max.mean[[s]]<-matrix(apply(temp.max[[s]],1,mean),ncol=1)
}

## parameters
params <- as.matrix(j.pheno.out)
param.mean <- apply(params,2,mean)
beta<-param.mean["betaTemp"]
q<-1/sqrt(param.mean["tau_add"])
## initial conditions
IC <-data$mu_ic  ##we don't have this? START @ END OF GCC TIME SERIES AND ITS UNCERTAINTY(sd) FOR EACH SITE

#phiend<-phenoforecast(IC,temp.max,beta,q,Nmc,gmin,gmax)
#next steps: compute confidence intervals, add in uncertainties 1 by one, do for 35 not 16, then set up for all sites,THEN assess where we're at

time=1:NT


#------THE FORECAST LOOP----------

site.pheno<-list()
#forecast loop
for (s in siteID){
  #uncertainties for each forecast
  prow<-sample.int(nrow(params),Nmc,replace=TRUE)
  Qmc<-1/sqrt(params[prow,"tau_add"])
  drow<-sample.int(ncol(temp.max[[s]]),Nmc,replace=TRUE)
  
  #forecast step
  site.pheno[[s]]<-phenoforecast(IC=IC.ens[[s]],
                                 tempcast=temp.max[[s]][,drow],
                                 beta=params[prow,"betaTemp"],
                                 Q=Qmc,
                                 n=Nmc,
                                 gmin=min(site.gcc[[s]]$gcc_90,na.rm=T),
                                 gmax=max(site.gcc[[s]]$gcc_90,na.rm=T))
}

##end forecast loop
#next steps: plotting each site with confidence intervals


##EVERYTHING BELOW THIS LINE IS OUR BART FORECAST PRACTICE
##########################################################
#---------------trying the deterministic---------
if(FALSE){
PhF.BART<-phenoforecast(IC=IC,
                        tempcast=temp.max.mean$BART,
                        beta=param.mean["betaTemp"],
                        Q=0,
                        n=Nmc,
                        gmin=gmin,
                        gmax=gmax)



plot(0,0, xlim=c(0,NT),ylim=range(PhF.BART))
for (p in 1:Nmc){
  points(PhF.BART[p,],type="l",col=p)
}

#this will make confidence intervals
time.f<-1:NT
ci.PHF.BART <- apply(as.matrix(PhF.BART),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART))
ecoforecastR::ciEnvelope(time.f,ci.PHF.BART[1,],ci.PHF.BART[3,],col=col.alpha("lightBlue",0.6))

#-----------------
#initial condition ensemble created from last gcc observation point & sd
IC.ens<-rnorm(Nmc,tail(BART$gcc_90,1),tail(BART$gcc_sd,1))
PhF.BART.IC<-phenoforecast(IC=IC.ens,
                           tempcast=temp.max.mean$BART,
                           beta=param.mean["betaTemp"],
                           Q=0,
                           n=Nmc,
                           gmin=gmin,
                           gmax=gmax)

time.f<-1:NT
ci.PHF.BART.IC <- apply(as.matrix(PhF.BART.IC),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART.IC))
ecoforecastR::ciEnvelope(time.f,ci.PHF.BART.IC[1,],ci.PHF.BART.IC[3,],col=col.alpha("lightBlue",0.6))

#-----------------
#parameter uncertainty for beta
prow <- sample.int(nrow(params),Nmc,replace=TRUE)
PhF.BART.IP<-phenoforecast(IC=IC.ens,
                           tempcast=temp.max.mean$BART,
                           beta=params[prow,"betaTemp"],
                           Q=0,
                           n=Nmc,
                           gmin=gmin,
                           gmax=gmax)

ci.PhF.BART.IP <- apply(as.matrix(PhF.BART.IP),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART.IP))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IP[1,],ci.PhF.BART.IP[3,],col=col.alpha("lightBlue",0.6))


#---------------driver uncertainty
drow<-sample.int(ncol(temp.max$BART),Nmc,replace=TRUE)
PhF.BART.IPT<-phenoforecast(IC=IC.ens,
                            tempcast=temp.max$BART[,drow],  #this is not working
                            beta=params[prow,"betaTemp"],
                            Q=0,
                            n=Nmc,
                            gmin=gmin,
                            gmax=gmax)
ci.PhF.BART.IPT <- apply(as.matrix(PhF.BART.IPT),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART.IPT))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IPT[1,],ci.PhF.BART.IPT[3,],col=col.alpha("lightBlue",0.6))

#----------------process error
Qmc <- 1/sqrt(params[prow,"tau_add"])
PhF.BART.IPTP<-phenoforecast(IC=IC.ens,
                             tempcast=temp.max$BART[,drow],  #this is not working
                             beta=params[prow,"betaTemp"],
                             Q=Qmc,
                             n=Nmc,
                             gmin=gmin,
                             gmax=gmax)
ci.PhF.BART.IPTP <- apply(as.matrix(PhF.BART.IPTP),2,quantile,c(0.025,0.5,0.975))
plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART.IPTP))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IPTP[1,],ci.PhF.BART.IPTP[3,],col=col.alpha("lightBlue",0.6))

plot(0,0,xlim=c(0,NT),ylim=range(PhF.BART.IPTP))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IPTP[1,],ci.PhF.BART.IPTP[3,],col=col.alpha("lightBlue",0.6))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IP[1,],ci.PhF.BART.IP[3,],col=col.alpha("green",0.6))
ecoforecastR::ciEnvelope(time.f,ci.PhF.BART.IPT[1,],ci.PhF.BART.IPT[3,],col=col.alpha("thistle3",0.6))
ecoforecastR::ciEnvelope(time.f,ci.PHF.BART.IC[1,],ci.PHF.BART.IC[3,],col=col.alpha("red2",0.6))
#ecoforecastR::ciEnvelope(time.f,ci.PhF.BART[1,],ci.PhF.BART[3,],col=col.alpha("thistle3"))
}
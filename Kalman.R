
##'  Kalman Filter
##' @param  M   = model matrix
##' @param  mu0 = initial condition mean vector
##' @param  P0  = initial condition covariance matrix
##' @param  Q   = process error covariance matrix
##' @param  R   = observation error covariance matrix
##' @param  Y   = vector of observations at the time
##'
##' @return list
##'  mu  = state mean vector for (a)nalysis and (f)orecast steps
##'  P    = state covariance matrix for a and f
KalmanFilter2 <- function(mu0,P0,Q,R,Y,tempcast,beta,gmin,gmax){
  
  ## storage
  nt = 16
  
  ## initialization
  I = diag(1,1)
  
  ## Analysis step: combine previous forecast with observed data
  KA <- KalmanAnalysis(mu0,P0,Y,R,H=I,I)
  
  ## run updates sequentially for each observation.
  IC.ensKF<-rnorm(Nmc, KA$mu, sqrt(KA$P))
    
  ## Forecast step: predict to next step from current
  KF <- phenoforecast(IC.ensKF,tempcast,beta,Q,n=Nmc,gmin,gmax)
  
  return(cbind(IC.ensKF,KF))
}

##' Kalman Filter: Analysis step
##' @param  mu.f = Forecast mean (vector)
##' @param  P.f  = Forecast covariance (matrix)
##' @param  Y    = observations, with missing values as NAs) (vector)
##' @param  R    = observation error covariance (matrix)
##' @param  H    = observation matrix (maps observations to states)
KalmanAnalysis <- function(mu.f,P.f,Y,R,H,I){
  obs = !is.na(Y) ## which Y's were observed?
  if(any(obs)){
    H <- H[obs,]                                              ## observation matrix
    K <- P.f %*% t(H) %*% solve(H%*%P.f%*%t(H) + R[obs,obs])  ## Kalman gain
    mu.a <- mu.f + K%*%(Y[obs] - H %*% mu.f)                  ## update mean
    P.a <- (I - K %*% H)%*%P.f                                ## update covariance
    ## Note: Here's an alternative form that doesn't use the Kalman gain
    ## it is less efficient due to the larger number of matrix inversions (i.e. solve)
    ## P.a <- solve(t(H)%*%solve(R[obs,obs])%*%(H) + solve(P.f))                             
    ## mu.a <- P.a %*% (t(H)%*%solve(R[obs,obs])%*%Y[obs] + solve(P.f)%*%mu.f)
  } else {
    ##if there's no data, the posterior is the prior
    mu.a = mu.f
    P.a = P.f
  }
  return(list(mu.a=mu.a,P.a=P.a))
}



#getting all IC's for each site:
start.date = "2021-04-15"
IC.ens<-list()
for (s in siteID){
  doy = which(site.gcc[[s]]$time == start.date)
  IC.ens[[s]]<-rnorm(Nmc,site.gcc[[s]]$gcc_90[doy],site.gcc[[s]]$gcc_sd[doy])
}

s="BART"
ENKFlist<-list()

#for (s in siteID){
  #uncertainties for each forecast
  Y = site.gcc[[s]]$gcc_90[doy]  ## note: are double dipping on IC and first Y
  
  ## initial conditions
  doy = which(site.gcc[[s]]$time == start.date)
  mu0 = site.gcc[[s]]$gcc_90[doy]
  P0 = (site.gcc[[s]]$gcc_sd[doy])^2 ## squared to make a variance
    
  
  ## parameters
  load(paste0("MCMC/",s,".Rdata"))
  params <- as.matrix(j.pheno.out)
  prow<-sample.int(nrow(params),Nmc,replace=TRUE)
  beta=params[prow,"betaTemp"]
  Q <- as.matrix(1/sqrt(params[prow,"tau_add"]))
  R = as.matrix((site.gcc[[s]]$gcc_sd[doy])^2)  ## square to variance
  gmin=min(site.gcc[[s]]$gcc_90,na.rm=T)
  gmax=max(site.gcc[[s]]$gcc_90,na.rm=T)
  
  ## met
  drow<-sample.int(ncol(temp.max[[s]]),Nmc,replace=TRUE)
    

###RELEVANT AGAIN

## Run Kalman Filter
  ENKFlist[[s]] <- list()
  ENKFlist[[s]][[1]] = KalmanFilter2(mu0 = mu0,
                               P0 = P0,
                               Q = Q,
                               R = R,
                               Y = Y,
                               tempcast = temp.max[[s]][,drow],
                               beta = beta,
                               gmin = gmin,
                               gmax = gmax)

for(t in 2:5){
  fx =  ENKFlist[[s]][[t-1]][,2] #last forecast we made of "today"
  
  ENKFlist[[s]][[t]]= KalmanFilter2(mu0=mean(fx),
                                    P0 = var(fx),
                                    Q = Q,
                                    R = as.matrix((site.gcc[[s]]$gcc_sd[doy+t-1])^2),
                                    Y = site.gcc[[s]]$gcc_90[doy+t-1],
                                    tempcast = temp.max[[s]][,drow],  ### need to fix
                                    beta = beta,
                                    gmin=gmin,
                                    gmax=gmax)
}

### plot ANALYSIS mean & CI time-series
par(mfrow=c(3,1))
for(i in 1:6){
  for(t in 1:5){
    mu=task5list[[t]]$mu
    P = task5list[[t]]$P
    nt = ncol(mu)
    time=1:nt -1
    ci = rbind(mu[i,]-1.96*sqrt(P[i,i,]),mu[i,]+1.96*sqrt(P[i,i,])) ##calculate ci from ensemble QUANTILES
    if(t == 1){
      plot(time,mu[i,],ylim=range(ci,na.rm=TRUE),type='n',main=states[i])
    }
    ecoforecastR::ciEnvelope(time+t,ci[1,],ci[2,],col=t)
    lines(time+t,mu[i,],col=34)
  }
  lines(time,Y[i,1:17])
}
#}
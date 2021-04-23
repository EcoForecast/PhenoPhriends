
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
KalmanFilter2 <- function(M,mu0,P0,Q,R,Y){
  
  ## storage
  nstates = 1
  nt = 16
  mu  = matrix(NA,nstates,nt+1)  ## forecast mean for time t
  #mu.a  = matrix(NA,nstates,nt)  ## analysis mean for time t
  P  = array(NA,c(nstates,nstates,nt+1))  ## forecast variance for time t
  #P.a  = array(NA,c(nstates,nstates,nt))  ## analysis variance for time t
  
  ## initialization
  I = diag(1,nstates)
  
  ## Analysis step: combine previous forecast with observed data
  KA <- KalmanAnalysis(mu0,P0,Y,R,H=I,I)
  mu[,1] <- KA$mu
  P[,,1] <- KA$P
  
  ## run updates sequentially for each observation.

    
    IC.ensKF<-list()
    for (s in siteID){
      IC.ensKF[[s]]<-rnorm(Nmc, mu[,1], sqrt(P[,,1]))
    }
    
    ## Forecast step: predict to next step from current
    KF <- phenoforecast(IC.ensKF,tempcast,beta,Q,n=Nmc,gmin,gmax)
    mu[,t+1] <- KF$mu
    P[,,t+1] <- KF$P

  
  return(list(mu=mu,P=P))
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

##' Kalman Filter: Forecast Step
##' @param mu.a = analysis posterior mean (vector)
##' @param P.a  = analysis posterior covariance (matrix)
##' @param M    = model (matrix)
##' @param  Q   = process error covariance (matrix)
KalmanForecast <- function(mu.a,P.a,M,Q){
  mu.f = M%*%mu.a
  P.f  = Q + M%*%P.a%*%t(M)
  return(list(mu.f=mu.f,P.f=P.f))
}

y = site.pheno$BART

adj = matrix(c(0,1,1,1,1,0,    ### state-to-state spatial adjacency (self=0)
               1,0,1,0,0,0,
               1,1,0,0,0,0,
               1,0,0,0,1,1,
               1,0,0,1,0,0,
               0,0,0,1,0,0),nstates,nstates,byrow=TRUE)

##IRRELEVANT FROM HERE DOWN
##```{r}
## log transform data
Y   = log10(y)

## options for process model 
#alpha = 0        ## assume no spatial flux
alpha = 0.05    ## assume a large spatial flux
M = adj*alpha + diag(1-alpha*apply(adj,1,sum))  ## random walk with flux

## options for process error covariance
Q = tau_proc            ## full process error covariance matrix
#Q = diag(diag(tau_proc))        ## diagonal process error matrix

## observation error covariance (assumed independent)  
R = diag(tau_obs,nstates) 

## prior on first step, initialize with long-term mean and covariance
mu0 = apply(Y,1,mean,na.rm=TRUE)
P0 = cov(t(Y),use="pairwise.complete.obs")
#w <- P0*0+0.25 + diag(0.75,dim(P0)) ## iptional: downweight covariances in IC
#P0 = P0*w 

###RELEVANT AGAIN

task5list<-list()

## Run Kalman Filter
task5list[[1]] = KalmanFilter2(M,mu0,P0,Q,R,Y[,1])

for(t in 2:5){
  
  task5list[[t]]= KalmanFilter2(M,task5list[[t-1]]$mu[,2],task5list[[t-1]]$P[,,2],Q,R,Y[,t])
  
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
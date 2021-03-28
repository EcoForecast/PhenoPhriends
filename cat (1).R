cat<-"
model{

### Loop over years:
for(i in 1:years){

  ### Data Model: GCC
  for(t in 1:180){
    y[i,t] ~ dnorm(x[i,t], tau_gcc)
  }

  ###Process Model
  for(t in 2:180){
    Gnew[i,t] <- x[i,t-1] ##+ betaTemp*z[i,t-1]
    x[i,t] ~ dnorm(Gnew[i,t],tau_add) T(gmin,gmax)
  }
  x[i,1] ~ dnorm(mu_ic,tau_ic)
}

### Priors
  tau_add ~ dgamma(a_add, r_add)
  tau_gcc ~ dgamma(a_gcc, r_gcc) ## had previousl been dnorm
  ##betaTemp ~ dnorm(mean_temp, precision_temp)
  ###mu ~ dnorm(mean_gcc, prec_gcc)
}"


STEI = dat[dat$siteID=="STEI",]
doy = lubridate::yday(STEI$time)
STEI = cbind(STEI,doy)
time <- as.Date(STEI$time)

## organize GCC data by year
year=(lubridate::year(STEI$time))
years = unique(year)
y=matrix(NA,length(years),366) #rows is years, columns is doy
for(i in 1:nrow(STEI)){
  y[as.numeric(as.factor(year))[i],STEI$doy[i]]=STEI$gcc_90[i]
}
y = y[-(1:2),] ##for this site, there's no data in year 1 or 2
years = years[-(1:2)]

## informative prior on obs error
yref = na.omit(as.vector(y[,355:366])) ## winter period of no change
a_gcc=length(yref)/5
r_gcc=a_gcc*var(yref)

## organize data and set priors & constants
data <- list(y=y,years=nrow(y),
             mu_ic=quantile(y,0.05,na.rm = TRUE),tau_ic=100,
             a_gcc=a_gcc,r_gcc=r_gcc,
             a_add=0.001,r_add=0.00001,
             gmin = min(y,na.rm=TRUE),
             gmax = max(y,na.rm=TRUE))

#defining initial state of model parameters
nchain <- 3
init <- list()
for(i in 1:nchain){
  ## set initial X to the average year
  init[[i]] <- list (x=matrix(apply(y[,1:180],2,mean,na.rm=TRUE),
                              nrow=nrow(y),ncol = 180,byrow = TRUE))
}

## compile
j.pheno.model.test <- rjags::jags.model (file = textConnection(cat),
                                  data = data,
                                  inits = init,
                                  n.chains = 3)
## check burn-in
j.pheno.out <- rjags::coda.samples (model = j.pheno.model.test,
                             variable.names = c("tau_add","tau_gcc"),
                             n.iter = 5000)    
plot(j.pheno.out)
coda::gelman.diag(j.pheno.out)
coda::effectiveSize(j.pheno.out)

## sample posterior
j.pheno.out <- rjags::coda.samples (model = j.pheno.model.test,
                                    variable.names = c("tau_add","tau_gcc","x"),
                                    n.iter = 5000)    #FOR ACTUAL RUN LETS CHANGE IT TO >5000


#### Helper function to parse JAGS variable names that include matrix syntax (e.g. "x[40,13]")
##' @param w mcmc object containing matrix outputs
##' @param pre prefix (variable name) for the matrix variable to be extracted
##' @param numeric boolean, whether to coerce class to numeric
parse.MatrixNames <- function(w, pre = "x", numeric = FALSE) {
  w <- sub(pre, "", w)
  w <- sub("[", "", w, fixed = TRUE)
  w <- sub("]", "", w, fixed = TRUE)
  w <- matrix(unlist(strsplit(w, ",")), nrow = length(w), byrow = TRUE)
  if (numeric) {
    class(w) <- "numeric"
  }
  colnames(w) <- c("row", "col")
  return(as.data.frame(w))
} # parse.MatrixNames

## calculate CI of X's
out <- as.matrix(j.pheno.out)
xf <- grep("^x",colnames(out))
ci <- apply((out[,xf]),2,quantile,c(0.025,0.5,0.975))
ci.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
doy = 1:180

## plot each year
for(i in seq_len(nrow(y))){
  sel = which(ci.names$row == i)
  plot(doy,ci[2,sel],type='n',ylim=range(ci[,sel],na.rm=TRUE),ylab="gcc",main=years[i])
  ecoforecastR::ciEnvelope(doy,ci[1,sel],ci[3,sel],col="lightBlue")
  points(doy,data$y[i,doy],pch="+",cex=0.5)
}

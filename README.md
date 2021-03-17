# PhenoPhriends
2021 BU EE585 course project: EFI/NEON phenology forecast


Name: Mira Kelly-Fair
Email: mirakf@bu.edu
Phone number: (415)312-4236

Name: Charlotte Malmborg
Email: malmborg@bu.edu
Phone number: 802-310-4177

Name: Samuel Agate 
Email: sagate@bu.edu
Phone Number: 510-559-0234

Name: Devin Hubbard
Email: dhubbard@bu.edu
Phone Number: 978-460-2546


Updating Data:
```
MAILTO=malmborg@bu.edu
15 18 * * * /EcoForecast/PhenoPhriends/Milestone4.Rmd

```


Milestone 5: 
```
#### Data Model
for(t in 1:n){
OBS[t] ~ dnorm(x[t],tau_obs)
Xf[t,1] ~ dnorm(muXf[1],tauXf[1])
}

The data model uses a density normal model to map the values of gcc_90 with a standard error of the observation error. The data model predicts the present gcc_90 using a density normal sample from our model along with a prediction (tauXf) of the standard error of the predicted precision.



#### Process Model
for(t in 2:n){
mu[t] <- x[t-1]  + betaX*x[t-1] + betaIntercept*Xf[t,1]
x[t]~dnorm(mu[t],tau_add)
}

The process model finds gcc_90 for the present day using gcc_90 data from the previous day. X[t-1] represents the gcc_90 from the previous day. To predict the next day, a coefficient betaX is multiplied by the previous day’s data to find the change, and then that is added to the previous day’s data. Similarly, a coefficient betaintercept  is multiplied by the predicted output of the data model and also added. Mu[t] is then used as the mean in a normal distribution given our process error (tau_add) to find a predicted distribution of gcc_90 for the day t.



#### Priors
x[1] ~ dnorm(x_ic,tau_ic)    ## initial GCC 
tau_obs ~ dgamma(a_obs,r_obs)  ## observation error: Error in calculating daily gcc 
tau_add ~ dgamma(a_add,r_add)  ## process error: green-up 

```


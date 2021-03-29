#FYI I  ran the download.R script to get dat first...

  
  #will need lat and long for each site to collect daymetr temp data
  #put them here so they were easy to find:
  #HARV lat:42.537 long:-72.173
  #BART lat:44.0639 long:-71.2874
  #SCBI lat:38.893 long:-78.140
  #STEI lat:45.509 long:-89.586
  #UKFS lat:39.040 long:-95.192
  #GRSM lat:35.689 long:-83.502
  #DELA lat:32.542 long:-87.804
  #CLBJ lat:33.401 long:-97.570

#make objects of siteID and lat/long
siteID <- c('BART','CLBJ','DELA','GRSM','HARV','SCBI','STEI','UKFS')
latt<-c(44.0639,33.401,32.542,35.689,42.537,38.893,45.509,39.040)
long<-c(-71.2874,-97.570,-87.804,-83.502,-72.173,-78.140,-89.586,-95.192)
#bind into data frame to reference in loop
sites<-as.data.frame(cbind(siteID,latt,long))

#put daymet data into a list
dm <- list()
for(i in 1:8){
  dm[[i]] <- daymetr::download_daymet(site = as.character(sites$siteID[i]),
                                      lat = sites$latt[i],
                                      lon = sites$long[i],
                                      start = 2016,
                                      end = 2020,
                                      internal = TRUE)
}

#can use any dm# to get day of year
doy <- dm[[1]]$data$yday

#grabbing max temp from each site's daymet data in dm list
maxtemp<-matrix(NA,nrow=length(doy),ncol=8)
for (i in 1:8){
  maxtemp[,i]<-dm[[i]]$data$tmax..deg.c.
}

#add the doy to be the first column, each of the next columns 2-9 are each site (in alphabetical order)
maxtemp<-cbind(doy,maxtemp)
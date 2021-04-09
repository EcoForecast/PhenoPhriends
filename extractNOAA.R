library('fs')
library(tidyverse)
library(ncdf4)

siteID= c('BART','CLBJ','DELA','GRSM','HARV','SCBI','STEI','UKFS')
base='Downloads/noaa/NOAAGEFS_1hr/'
mid='/00/'
date=(Sys.Date()-1)

#old code: makes df1
#directory=paste0(base,siteID,'/',date,mid)
#files=dir_ls(directory)

# l.files=length(files)
# df1 <-as.data.frame(matrix(nrow=1000,ncol=385))
# for (i in  1:l.files){
#   cat=nc_open(files[i])
#   df1[i,]=ncvar_get(cat,'air_temperature')
# }
# df1=na.omit(df1)

#new code as of 4/12, makes df1 into list with NOAA data from all sites
df1 <-list()
for (s in seq_along(siteID)){
  directory=paste0(base,siteID[s],'/',date,mid)
  files=dir_ls(directory)

l.files=length(files)
df1[[s]] <-as.data.frame(matrix(nrow=l.files,ncol=385))
  for (i in  1:l.files){
    cat=nc_open(files[i])
    df1[[s]][i,]=ncvar_get(cat,'air_temperature')
    nc_close(cat)
    df1[[s]]=na.omit(df1[[s]])
  }
#df1[[s]]=na.omit(df1[[s]])
}

names(df1)#=siteID

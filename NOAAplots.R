
df1_t <- as.data.frame(t(as.matrix(df1)))
colnames(df1_t) = files
  
df.BART = df1_t %>% select(contains('BART'))
df.CLBJ = df1_t %>% select(contains('CLBJ'))
df.DELA = df1_t %>% select(contains('DELA'))
df.GRSM = df1_t %>% select(contains('GRSM'))
df.HARV = df1_t %>% select(contains('HARV'))
df.SCBI = df1_t %>% select(contains('SCBI'))
df.STEI = df1_t %>% select(contains('STEI'))
df.UKFS = df1_t %>% select(contains('UKFS'))


##old.par <- par(mfrow=c(8, 1))

plot(df.BART[,1], type = 'l', main = "BART", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.BART)){
lines(df.BART[,i], type = 'l')
}

plot(df.CLBJ[,1], type = 'l', main = "CLBJ", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.CLBJ)){
  lines(df.CLBJ[,i], type = 'l')
}

plot(df.DELA[,1], type = 'l', main = "DELA", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.DELA)){
  lines(df.DELA[,i], type = 'l')
}

plot(df.GRSM[,1], type = 'l', main = "GRSM", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.GRSM)){
  lines(df.GRSM[,i], type = 'l')
}

plot(df.HARV[,1], type = 'l', main = "HARV", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.HARV)){
  lines(df.HARV[,i], type = 'l')
}

plot(df.SCBI[,1], type = 'l', main = "SCBI", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.SCBI)){
  lines(df.SCBI[,i], type = 'l')
}

plot(df.STEI[,1], type = 'l', main = "STEI", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.STEI)){
  lines(df.STEI[,i], type = 'l')
}

plot(df.UKFS[,1], type = 'l', main = "UKFS", xlab = "Time", ylab = "Temperature (K)")
for(i in 1:length(df.UKFS)){
  lines(df.UKFS[,i], type = 'l')
}


##par(old.par)














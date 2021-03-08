

#this is a lot but we got tired of for loops :/

BART <- dat[ which(dat$siteID== "BART"), ]
CLBJ <- dat[ which(dat$siteID== "CLBJ"), ]
DELA <- dat[ which(dat$siteID== "DELA"), ]
GRSM <- dat[ which(dat$siteID== "GRSM"), ]
HARV <- dat[ which(dat$siteID== "HARV"), ]
SCBI <- dat[ which(dat$siteID== "SCBI"), ]
STEI <- dat[ which(dat$siteID== "STEI"), ]
UKFS <- dat[ which(dat$siteID== "UKFS"), ]

old.par <- par(mfrow=c(4, 2))

plot(BART$time, BART$gcc_90, type = "l", main = "BART")
plot(CLBJ$time, CLBJ$gcc_90, type = "l", main = "CLBJ")
plot(DELA$time, DELA$gcc_90, type = "l", main = "DELA")
plot(GRSM$time, GRSM$gcc_90, type = "l", main = "GRSM")
plot(HARV$time, HARV$gcc_90, type = "l", main = "HARV")
plot(SCBI$time, SCBI$gcc_90, type = "l", main = "SCBI")
plot(STEI$time, STEI$gcc_90, type = "l", main = "STEI")
plot(UKFS$time, UKFS$gcc_90, type = "l", main = "UKFS")

par(old.par)




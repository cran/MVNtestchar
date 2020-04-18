## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----echo=FALSE---------------------------------------------------------------
pos.def.plot <- function(npoints, v12){
   nv12 <- length(v12)
   x <- rep(1:npoints,each=npoints) /npoints
   y <- rep(1:npoints,times=npoints) /npoints
   v12 <- rep(v12,each=npoints*npoints)
   nameV12 <- paste("|V12| =",v12)
   calcpd <- x*y - v12*v12
   posdef <- calcpd > 0
   xdf <- data.frame(x,y,v12,calcpd,posdef,nameV12)
   xdf[xdf$posdef,]
}

   tempdf2 <- pos.def.plot(npoints=40,v12=c(.15,.30,.45,.60,.75,.9))
   horlabel="V1"
   vertlabel="V2"
   legendname=NULL
   cap=NULL
   mtitle="Support for Multivariate Normality Test"
   stitle="Positive Definite Region for p=2"
	 XVAR <- tempdf2[,1]
	 YVAR <- tempdf2[,2]
	 COV1 <- tempdf2[,5]
	 dfplot <- data.frame(XVAR,YVAR,COV1)
	 FACET <- tempdf2[,6]
	 dfplot <- data.frame(dfplot,FACET)
   out <- ggplot2::ggplot(data=dfplot,ggplot2::aes(XVAR,YVAR,COV1)) + ggplot2::geom_point()
	 out$labels$shape <- legendname
	 names(FACET)<-tempdf2$namesV12
	 out <- out + ggplot2::facet_wrap(~FACET)
   out <- out + ggplot2::ggtitle(mtitle,subtitle=stitle) + ggplot2::xlab(horlabel) + ggplot2::ylab(vertlabel) +                                  ggplot2::labs(caption=cap,legend=legendname)
	 out <- out + ggplot2::labs(color=legendname)
	 print(out)

## ----echo=FALSE---------------------------------------------------------------
  a <- c(2,5,10,15,20,16,250,2000,6750,16000,44.4,44.4,44.4,44.4,44.4,7,111,889,3000,7110)
  a <- c(a,2,5,6,7,9,512,125000,373248,941192,4251528,14.8,7.2,7.8,7.8,7.8,76,8948,29213,73688,331619)
  a <- c(a,2,3,4,5,6,65536,3779136,67108864,630000000,3900000000,.6,.5,.5,.5,.5,344,20410,335544,3000000,20000000)
  a <- matrix(a,nrow=5,byrow=FALSE)
  b <- matrix("     ",nrow=5,ncol=1)
  c <- matrix("  ",nrow=5,ncol=1)
  a <- cbind(a,b,c)
  a <- as.data.frame(a, row.names=NULL, optional=TRUE, col.names=NULL)
  a <- a[,c(14,1:4,13,5:8,13,9:12)]
  a
  

## ----echo=FALSE---------------------------------------------------------------
    a <- c(2,5,10,15,20,256,4000,31997,107989,255974,427,6666,53328,179982,426624)
  a <- c(a,2,5,6,7,9,3648, 429504,1402224,3537024,15917712,6080,715840,2337040,5395040,26529520)
  a <- c(a,2,3,4,5,6,20640,1224600,20132640,187000000,1190000000,34400,2041000,33554400,312000000,1980000000)
  a <- matrix(a,nrow=5,byrow=FALSE)
  b <- matrix("   ",nrow=5,ncol=1)
  c <- matrix("         ",nrow=5,ncol=1)
    a <- cbind(a,b,c)
  a <- as.data.frame(a, row.names=NULL, optional=TRUE, col.names=NULL)
  a <- a[,c(11,1:3,10,4:6,10,7:9)]
  a


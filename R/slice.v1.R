slice.v1 <-
function(level3 =.6, theta=0, phi=60, inc=25, lseq=100, ticktype="detailed", diagnose=FALSE, verbose=TRUE)
{
	#					slice.v1
	#
	# VALUE	Rotatable plot of slice through support region in positive definite 2 x 2 matrix at fixed value of off-diagonal variable. 
	#
	# INPUT	level3	Value of V1 (0 < level3 < 1)
	#		theta, phi	Initial angles of rotation of plot
	#		inc		Increment in degrees for rotation of plot
	#		lseq		Number of points in each of x and y
	#		ticktype	Character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; 
	#					"detailed" draws normal ticks as per 2D plots.
	#
	#		diagnose	Logical. T causes printing of diagnostic content
	#		verbose	Logical. T causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote = F)
		print("Running slice.v1", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
		print("Call:", quote = F)
		print(MC)
		print("", quote = F)
	}
	#############################################
	# Program begins here                       #
	# Set up diagonal element grids for x and y #
	#############################################
	cycle <- TRUE
 	x <- seq(-1, 1, length.out = 2*lseq)
 	y <- x
#	x <- x[x>=0]
	y <- y[y>=0]
	z <- matrix(level3, nrow=length(x),ncol=length(y))
	for(i in 1:length(x)){
		for(j in 1:length(y)){
			if(x[i]^2 >= y[j] * level3) z[i,j] <- NA		# x is V3 and z is V1 
		}
	}
	z[1,1] <- 1
	z[1,2] <- 0
	oldpar <- graphics::par(bg = "white")
	#
	#################################################################
	# Cycle in order to allow interactive input on rotation of plot #
	#################################################################
	td <- theta
	ph <- phi
	mt <- paste("Possible values of (V2, V12) for V1 =",level3,"\nas slice through support region of \n2x2 positive definite matrices")
	while(cycle){
	 	graphics::persp(x, y, z, theta = td, phi = ph, expand = 0.5, col = "lightblue",ticktype=ticktype, xlab="V12",ylab="V2",zlab="V1")
		graphics::title(sub=".")## work around persp+plotmath bug
 		graphics::title(main = mt)
		print("Click on R Console and then",quote=F)
		print("", quote = F)
		print("press 'u' to roll view up", quote=F)
		print("      'd' to roll view down", quote=F)
		print("      'r' to roll view to right", quote=F)
		print("      'l' to roll view to left", quote=F)
		print("      'x' to fix position of graph", quote=F)
		print("", quote = F)
		print("After each letter, press 'Enter'", quote=F)
		vv <- readline("Waiting for you . . . ")
		if(vv == "u") ph <- ph - inc
		if(vv == "d") ph <- ph + inc
		if(vv == "r") td <- td - inc
		if(vv == "l") td <- td + inc
		if(vv == "x"){
			cycle <- FALSE
			break
		}
	}
	graphics::par(oldpar)
	#
	if(verbose) {
		print("", quote = F)
		print("Finished running slice.v1", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
	}
 	list(level3=level3, theta=td, phi=ph, inc=inc, lseq=lseq,ticktype=ticktype, Call=MC)


}

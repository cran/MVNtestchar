support.p2 <-
function(theta=110, phi=10, lseq=150, inc=25, ticktype="detailed", diagnose=FALSE, verbose=TRUE)
{
	#					support.p2
	#
	# VALUE    Rotatable plot of support region for positive definite matrix with p=2. Output list contains latest parameters for display.
	#
	# INPUT	theta, phi 	Angle in degrees for view of plot
	#		lseq		Number of points in V1 or V2
	#		inc		Angle in degrees to increment view of plot
	#		ticktype	Character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; 
	#					"detailed" draws normal ticks as per 2D plots.
	#
	#		diagnose	Logical. T causes printing of diagnostic content
	#		verbose	Logical. T causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote = F)
		print("Running support.p2", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
		print("Call:", quote = F)
		print(MC)
		print("", quote = F)
	}
	######################################################
	# Function relating off-diagonal element to diagonal #
	# elements of 2 x 2 matrix                           #
	######################################################
	simpleprod <- function(x,y){sqrt(x*y)}

	#############################################
	# Program begins here                       #
	# Set up diagonal element grids for x and y #
	#############################################
	cycle <- TRUE
 	x <- seq(-1, 1, length.out = lseq)
	x <- rep(x, each=2)
	index <- 2*(1:(length(x)/2))
	########################################################################################
	# Grid points x and y must be increasing; multiply every other one by a small fraction #
	########################################################################################
	x[index] <- x[index]*1.00001 
	y <- x

	x <- x[x>=0]
	y <- y[y>=0]
	z <- outer(x,y,simpleprod)
	################################################
	# Add negative values of off-diagonal variable #
	################################################
	uu <- sample(c(-1,1),size=length(x)^2,replace=TRUE)
 	mult <- matrix(uu, nrow=dim(z)[1], ncol=dim(z)[2])
	z <- z * mult
	oldpar <- graphics::par(bg = "white")

	#################################################################
	# Cycle in order to allow interactive input on rotation of plot #
	#################################################################
	td <- theta
	ph <- phi
	while(cycle){

	 	graphics::persp(x, y, z, theta = td, phi = ph, expand = 0.5, col = "lightblue",ticktype=ticktype, xlab="V1", ylab="V2",zlab="V12")
		graphics::title(sub=".")## work around persp+plotmath bug
 		graphics::title(main = "Support region for 2x2 positive definite matrices \nwith diagonal values V1, V2 <= 1")

		print("Click on R Console and then",quote=F)
		print("", quote = F)
		print("Press 'u' to roll view up", quote=F)
		print("      'd' to roll view down", quote=F)
		print("      'r' to roll view to right", quote=F)
		print("      'l' to roll view to left", quote=F)
		print("      'x' to exit program", quote=F)
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
		print("Finished running support.p2", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
	}
	list(theta=td, phi=ph, inc=inc, lseq=lseq, ticktype=ticktype, Call=MC)
}

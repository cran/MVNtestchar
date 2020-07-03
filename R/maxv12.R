maxv12 <-
function(theta=30, phi=30, inc=25, lseq=200, ticktype="detailed", diagnose=FALSE, verbose=TRUE)
{
	#					maxv12
	#
	# VALUE	Rotatable plot of surface of possible values of off-diagonal variable in positive definite 2 x 2 matrix. 
	#			Output includes last values of plot rotation.
	#
	# INPUT	theta, phi	Angles of rotation of plot
	#		inc		Increment of angle rotation for view
	#		lseq		Number of points along V1 and V2
	#		ticktype	Character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; 
	#					"detailed" draws normal ticks as per 2D plots.
	#
	#		diagnose	Logical. T causes printing of diagnostic content
	#		verbose	Logical. T causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote = F)
		print("Running maxv12", quote = F)
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
	#
	#############################################
	# Program begins here                       #
	# Set up diagonal element grids for x and y #
	#############################################
	#######################################################
	##
	#######################################################
	cycle <- TRUE
 	x <- seq(-1, 1, length.out = lseq)
 	y <- x
	x <- x[x>=0]
	y <- y[y>=0]
	z <- outer(x,y,simpleprod)
 	oldpar <- graphics::par(bg = "white")
	#
	#################################################################
	# Cycle in order to allow interactive input on rotation of plot #
	#################################################################
	td <- theta
	ph <- phi
	while(cycle){

	 	graphics::persp(x, y, z, theta = td, phi = ph, expand = 0.5, col = "lightblue",ticktype="detailed",xlab="V1",ylab="V2",zlab="V12")
		graphics::title(sub=".")## work around persp+plotmath bug
 		graphics::title(main = "Maximum absolute value of off-diagonal variable V12 \nin 2x2 symmetric positive definite matrix\nas a function of diagonal variables, V1 and V2")

		print("Click on R Console and then",quote=F)
		print("", quote = F)
		print("press 'u' to roll view up", quote=F)
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
		print("Finished running maxv12", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
	}
	list(theta=td, phi=ph, inc=inc, lseq=lseq, ticktype=ticktype, Call=MC)
}

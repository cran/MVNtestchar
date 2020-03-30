maxv12 <-
function(theta=30, phi=30, inc=25, lseq=200, static=FALSE, ticktype="detailed", diagnose=FALSE, verbose=TRUE)
{
	#					maxv12
	#
	# VALUE	Rotatable plot of surface of possible values of off-diagonal variable in positive definite 2 x 2 matrix. 
	#			Output includes last values of plot rotation.
	#
	# INPUT	theta, phi	Angles of rotation of plot
	#		inc		Increment of angle rotation for view
	#		lseq		Number of points along V1 and V2
	#		static	Logical. TRUE suppresses rotation
	#		ticktype	Character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; 
	#					"detailed" draws normal ticks as per 2D plots.
	#
	#		diagnose	Logical. TRUE causes printing of diagnostic content
	#		verbose	Logical. TRUE causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote=FALSE)
		print("Running maxv12", quote=FALSE)
		print("", quote=FALSE)
		print(date(), quote=FALSE)
		print("", quote=FALSE)
		print("Call:", quote=FALSE)
		print(MC)
		print("", quote=FALSE)
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
	cycle <- TRUE
 	x <- seq(-1, 1, length.out = lseq)
 	y <- x
	x <- x[x>=0]
	y <- y[y>=0]
	z <- outer(x,y,simpleprod)
	oldpar <- graphics::par(no.readonly=TRUE)
	on.exit(graphics::par(oldpar))
	graphics::par(bg = "white")
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
		if(static){
			message("You ran this graph in static mode. To view a rotatable version, set static=FALSE")
			vv <- "x"
		}else{
		print("Click on R Console and then",quote=FALSE)
		print("", quote=FALSE)
		print("press 'u' to roll view up", quote=FALSE)
		print("      'd' to roll view down", quote=FALSE)
		print("      'r' to roll view to right", quote=FALSE)
		print("      'l' to roll view to left", quote=FALSE)
		print("      'x' to fix position of graph", quote=FALSE)
		print("", quote=FALSE)
		print("After each letter, press 'Enter'", quote=FALSE)
		vv <- readline("Waiting for you . . . ")
		}
		if(vv == "u") ph <- ph - inc
		if(vv == "d") ph <- ph + inc
		if(vv == "r") td <- td - inc
		if(vv == "l") td <- td + inc
		if(vv == "x"){
			cycle <- FALSE
			break
		}
	}
	if(verbose) {
		print("", quote=FALSE)
		print("Finished running maxv12", quote=FALSE)
		print("", quote=FALSE)
		print(date(), quote=FALSE)
		print("", quote=FALSE)
	}
	list(theta=td, phi=ph, inc=inc, lseq=lseq, ticktype=ticktype, Call=MC)
}

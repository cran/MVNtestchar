#' @export
slice.v12 <-
function(level3 =.3, theta=30, phi=10, inc=25, lseq=100, static=FALSE, ticktype="detailed", diagnose=FALSE, verbose=TRUE)
{
	#					slice.v12
	#
	# VALUE	Rotatable plot of slice with fixed value of off-diagonal variable in positive definite 2 x 2 matrix.
	#			Output includes list of latest settings of plot.
	#
	# INPUT		level3		Value of V12 (0 < level3 < 1)
	#		theta, phi	Initial angles of rotation of plot
	#		inc		Increment in angle of rotation of plot
	#		lseq		Number of points in each of x and y
	#		static		Logical. TRUE suppresses rotation.
	#		ticktype	Character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; 
	#					"detailed" draws normal ticks as per 2D plots.
	#
	#		diagnose	Logical. TRUE causes printing of diagnostic content
	#		verbose	Logical. TRUE causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote = FALSE)
		print("Running slice.v12", quote = FALSE)
		print("", quote = FALSE)
		print(date(), quote = FALSE)
		print("", quote = FALSE)
		print("Call:", quote = FALSE)
		print(MC)
		print("", quote = FALSE)
	}
	######################################################
	# Function relating off-diagonal element to diagonal #
	# elements of 2 x 2 matrix                           #
	######################################################
	singlevalue <- function(x,y){matrix(level3,nrow=length(x),ncol=length(y))}
	#
	#############################################
	# Program begins here                       #
	# Set up diagonal element grids for x and y #
	#############################################
	cycle <- TRUE
 	x <- seq(-1, 1, length.out = 2*lseq)
 	y <- x
	x <- x[x>=0]
	y <- y[y>=0]
	z <- matrix(level3, nrow=length(x),ncol=length(y))
	for(i in 1:length(x)){
		for(j in 1:length(y)){if(x[i] * y[j] <= level3^2) z[i,j] <- NA
		}
	}
	z[1,1] <- 1
	z[1,2] <- 0
	oldpar <- graphics::par(no.readonly=TRUE)
	on.exit(graphics::par(oldpar))
	graphics::par(bg = "white")
	#
	#################################################################
	# Cycle in order to allow interactive input on rotation of plot #
	#################################################################
	td <- theta
	ph <- phi
	mt <- paste("Possible values of (V1, V2) for |V12|=",level3,"\nas slice through support region of \n2x2 positive definite matrices")
	while(cycle){
	 	graphics::persp(x, y, z, theta = td, phi = ph, expand = 0.5, col = "lightblue",ticktype="detailed",xlab="V1",ylab="V2",zlab="V12")
		graphics::title(sub=".")## work around persp+plotmath bug
 		graphics::title(main = mt)
		if(static){
			message("You ran this graph in static mode. To view a rotatable version, set static=FALSE")
			vv <- "x"
		}else{
		print("Click on R Console and then",quote=FALSE)
		print("", quote = FALSE)
		print("press 'u' to roll view up", quote=FALSE)
		print("      'd' to roll view down", quote=FALSE)
		print("      'r' to roll view to right", quote=FALSE)
		print("      'l' to roll view to left", quote=FALSE)
		print("      'x' to fix position of graph", quote=FALSE)
		print("", quote = FALSE)
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
		print("", quote = FALSE)
		print("Finished running slice.v12", quote = FALSE)
		print("", quote = FALSE)
		print(date(), quote = FALSE)
		print("", quote = FALSE)
	}
	list(level3=level3, theta=td, phi=ph, inc=inc, lseq=lseq,ticktype=ticktype, Call=MC)
}

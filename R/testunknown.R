testunknown <-
function(x, pvector, k, diagnose.s=FALSE, diagnose=FALSE, verbose=TRUE)
{
	#					testunknown
	#
	# VALUE		Create matrices without nuisance parameters. Tabulate distribution.
	#			Output is list: data.frame of mobs simulations with r observations on each, goodness of fit chi square test and call to this function.
	#
	# INPUT		x		Name of n x p matrix or array with dimensions (x,y,z) = (n=4r(p+1),p, mobs). 
	#		pvector		Dimensionality of random vector
	#		k		Number of cuts in (0,1) for diagonal elements of each matrix.  Program uses 2k cuts for offdiagonal elements.
	#
	#		diagnose.s	Logical. TRUE causes printing of diagnostic terms in called function(s), primarily exploreM()
	#		diagnose	Logical. TRUE causes printing of diagnostic content of main function
	#		verbose		Logical. TRUE causes printing of program ID before and after running.
	#
	MC <- match.call()
	if(verbose) {
		print("", quote = F)
		print("Running testunknown", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
		print("Call:", quote = F)
		print(MC)
		print("", quote = F)
	}
	############################################################################################
	# This function calls the function exploreM(), which in turn calls the function formS().   #
	# The two explore functions are located within testunknown(), with formS() positioned first. #
	########################################################################################## #

	#########################	
	# n x n identity matrix #
	#########################	
	my.identity <- function(n){
		matrix(rep(c(1,rep(0,n)),times=n)[1:(n*n)],nrow=n,ncol=n,byrow=TRUE)
	}
	######################################################
	# Define the support function formS that removes the # 
	# nuisance parameters and calculates the test matrix #
	######################################################
	formS <- function(y,midgroup){
		U1 <- y[1:midgroup,]
		U2 <- y[midgroup + (1:midgroup),]
		U <- (U1 - U2)/sqrt(2)
			if(diagnose){
				Hmisc::prn(y)
				Hmisc::prn(U1)
				Hmisc::prn(U2)
				Hmisc::prn(U1-U2)
				Hmisc::prn(U)
			}
 		uprod <- array(0,dim=c(p,p,2*(p+1)))
		for(k in 1:(2*(p+1))){
			Uj <- matrix(U[k,],ncol=1)
			uprod[,,k] <- Uj %*% t(Uj)
		}
		uprod1 <- uprod[,,1:(p+1)]
		uprod2 <- uprod[,,(p+1+1):(2*(p+1))] 
			if(diagnose){
				Hmisc::prn(uprod)
				Hmisc::prn(uprod1)
				Hmisc::prn(uprod2)
			}
		W1 <- apply(uprod1,c(1,2),sum)
		W2 <- apply(uprod2,c(1,2),sum)
		T1 <- W1 + W2
		#
		################################################
		# Determine eigenvalues and eigenvectors of T1 #
		################################################
		eigenT1 <- eigen(x=T1,symmetric=TRUE)
		eigenvalues <- eigenT1[[1]]
		eigenvectors <- eigenT1[[2]]
			if(diagnose){
				checkeigen <- eigenvectors %*% t(eigenvectors)
				Hmisc::prn(W1)
				Hmisc::prn(W2)
				Hmisc::prn(T1)
				Hmisc::prn(eigenT1)
				Hmisc::prn(eigenvalues)
				Hmisc::prn(eigenvectors)
				Hmisc::prn(checkeigen)
			}
		lambda12inv <- lambda12 <- Ip <- my.identity(p)
		for(i in 1:p){
			lambda12[i,i] <- sqrt(eigenvalues[i]) 
			lambda12inv[i,i] <- 1/lambda12[i,i]
		}
		lambda <- lambda12 %*% lambda12
			if(diagnose){
				Hmisc::prn(lambda)
				Hmisc::prn(lambda12inv)
				Hmisc::prn(lambda12)
				check2 <- eigenvectors %*% lambda %*% t(eigenvectors)
				Hmisc::prn(check2)
				Hmisc::prn(T1)
			}
		#######################################
		# Calculate inverse square root of T1 #
		#######################################
		T1minus12 <- eigenvectors %*% lambda12inv %*% t(eigenvectors)
			if(diagnose){
				Hmisc::prn(T1minus12)
				check3 <- T1minus12 %*% T1 %*% t(T1minus12)
				Hmisc::prn(check3)
			}
		Si <- T1minus12 %*% W2 %*% t(T1minus12)
			if(diagnose)Hmisc::prn(Si)
		Si
	}
	#########################
	# End of function formS #
	#########################

	##########################################################################
	# Define the function exploreM, which creates the sample matrices Si and #
	# converts them to vector format for each layer of x.                    #
	##########################################################################
	exploreM <- function(x, k, diagnose=diagnose.s,verbose=verbose){
	#######################################
	# Eliminate all but 4r(p+1) rows of x #
	#######################################
		dimx <- dim(x)
		p <- dimx[2]
		r <- floor(dimx[1]/(4*(p+1))) 
			if(diagnose){
				Hmisc::prn(dimx)
				Hmisc::prn(p)
				Hmisc::prn(r)
				c1 <- 4*(p+1)
				c2 <- dimx[1]/c1
				Hmisc::prn(c1)
				Hmisc::prn(c2)
			}
		n <- 4*r*(p+1)
		nelim <- dimx[1] - n
		if(nelim > 0){
			elim <- -1 * sample.int(dimx[1], size = nelim, replace=FALSE)
			x <- x[elim,]		# abbreviated sample, with exactly n = 4r(p+1) rows
		}
			if(diagnose){
				print("After eliminating excess rows in x",quote=F)
				Hmisc::prn(dimx)
				Hmisc::prn(p)
				Hmisc::prn(r)
				c1 <- 4*(p+1)
				c2 <- dimx[1]/c1
				Hmisc::prn(c1)
				Hmisc::prn(c2)
			}
	#
	#######################################################################################
	# Make a list with r elements, each element of which is a data frame with 4(p+1) rows #
	#######################################################################################
		listx <- vector("list",r)
		listrows <- 4*(p+1)
		for(i in 1:r){
			if(diagnose){
				print("***************************************",quote=F)
				print(paste("Running element",i,"of",r,"in listx"),quote=F)
			}
			listx[[i]] <- x[1:listrows,]
			x <- x[-1*(1:listrows),]
		}
	#########################################################
	# Run the function formS on each element of listx       #
	# to determine the r sample matrices. Each matrix is    #
	# in the vector form of a symmetric matrix: p diagonal  #
	# values followed by the rest of each row, constituting #
	# the upper triangle of the matrix.                     #
	#########################################################
		midgroup <- 2*(p+1)
		cutgroup <- p+1
		runfn <- list(r)
		m <- p * (p + 1) / 2
		vector.rep <- matrix(-999,nrow=r, ncol=m)	
		for(k in 1:r){
			samples <- formS(listx[[k]],midgroup)		# a matrix Si that depends on k
	###############################################################
	# Create a vector representation of Si from the matrix sample #	
	###############################################################
			for(i in 1:p){
				vector.rep[k,i] <- samples[i,i] 
			}
			start <- p + 1		
			for(j in 1:(p-1)){
				thisrow <- samples[j,]
				thisrow <- thisrow[-1*(1:j)]
				lenthisrow <- length(thisrow)
				vector.rep[k,start:(start+lenthisrow-1)] <- thisrow
				start <- start + lenthisrow	
			}
		}
		if(verbose) {
			print("", quote = F)
			print("Finished running exploreM", quote = F)
			print("", quote = F)
			print(date(), quote = F)
			print("", quote = F)
		}
		vector.rep
	}
	############################
	# End of function exploreM #
	############################


################################################################ Main program starts here ###################################
	#########################################################
	# If x is a matrix, convert it to an array with 1 level #
	#########################################################
	dimx <- dim(x)
	lendimx <- length(dimx)
	if(lendimx == 2){
		print("Converting input matrix to an array with 1 level", quote=F)
		X <- array(NA, dim=c(dimx[1],dimx[2],1)) 
		X[,,1] <- x
	} else X <- x
	Xdims <- dim(X)
			if(diagnose)Hmisc::prn(Xdims)
	n <- Xdims[1]
	p <- Xdims[2]
	mobs <- Xdims[3]
	#
	################
	# QC the input #
	################
	if(p != pvector){ 
		uu <- paste("The input x is supposed to have", pvector, "columns")
		stop(uu)
	}
	r <- floor(n/(4*(p+1)))
	print(paste("The source file dimensions (n, p, mobs) are (", n,", ", p,", ", mobs, ")",sep="" ),quote=F)
	print(paste("resulting in r =",r,"adjusted observations after the mean and covariance matrix are removed."),quote=F)
			if(diagnose){
				Hmisc::prn(n)
				Hmisc::prn(p)
				Hmisc::prn(r)
				Hmisc::prn(mobs)
			}
	print("", quote=F)
	#######################################################################################
	# Prepare vector version of matrices and encode the subcubes to show where they occur #
	#######################################################################################
	iuu1 <- (k:1)*(1/k)
	iuu2 <- iuu1 - 1
	iuu <- c(iuu1,iuu2)			# iuu is in descending order
	codes <- paste("_",iuu,sep="")
	Mcodes <- data.frame(codes,iuu)
	m <- p * (p+1) / 2
	subcubes <- list(mobs)
	##########################################################
	# Run exploreM function on each set of r sample matrices #
	##########################################################
	out <- list(mobs)
	good <- list(mobs)
	for(kk in 1:mobs){
		z <- exploreM(x=X[,,kk], k=k, diagnose=diagnose.s,verbose=verbose)

		if(any(z > 1)) warning("There is a sample matrix with an element > 1")

		dimz1 <- dim(z)[1]
		uu <- matrix("A",nrow=dimz1,ncol=m)
		for(i in 1:dimz1){
			for(j in 1:p){
				for(nn in 1:k){
					index <- (1:dimz1)[z[,j] < Mcodes[nn,2]]
					uu[index,j] <- as.character(Mcodes[nn,1])
				}
			}
			for(j in (p+1):m){
				for(nn in 1:(2*k)){
					index <- (1:dimz1)[z[,j]<Mcodes[nn,2]]
					uu[index,j] <- as.character(Mcodes[nn,1])
				}
			}
		}
		subcube.code <- rep("AA",dimz1)
		for(i in 1:dimz1){
			zz <- NULL
			for(j in 1:m){
				zz <- paste(zz,uu[i,j],sep="")
			}
			subcube.code[i] <- zz
		}
		zz <- data.frame(subcube.code,z)
		###################################################
		# Tabulate the frequency distribution of matrices #
		###################################################
		unique.codes <- unique(subcube.code)
		lencodes <- length(unique.codes)
		sumcodes <- rep(0,lencodes)
		for(i in 1:lencodes){
			sumcodes[i] <- sum(subcube.code==unique.codes[i])
		}
		out[[kk]] <- data.frame(unique.codes,sumcodes)
		good[[kk]] <- stats::chisq.test(out[[kk]]$sumcodes)
	}
	if(verbose) {
		print("", quote = F)
		print("Finished running testunknown", quote = F)
		print("", quote = F)
		print(date(), quote = F)
		print("", quote = F)
	}
	list(Distribution=out, "Goodness of Fit"=good, Call=MC)
}

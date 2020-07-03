\name{support.p2}
\alias{support.p2}
\title{Show Support Region of Positive Definite Matrices with Rank 2}
\description{Rotatable plot of support region for positive definite matrix with p=2}
\usage{
support.p2(theta = 110, phi = 10, lseq = 150, inc = 25, ticktype="detailed", 
    diagnose = FALSE, verbose = TRUE)
}
\arguments{
  \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{inc}{increment in degrees of plot rotations}
  \item{ticktype}{simple or detailed ticks on variables}
  \item{diagnose}{Logical. T causes printing of diagnostic content}
  \item{verbose}{Logical. T causes printing of program ID before and after running}
}
\details{
Support region for p-variate positive definite matrix distributions is difficult to envision except for p=2. The diagonals
    of the matrix are V1 and V2 and the off-diagonal variable is V12. In our application 0<=V1,V2<=1, and -1<=V12<=1,
    so the bounded space is a hyper-rectangle. Each point in this region represents a symmetric pxp matrix, but not all
    of these are positive definite. This function shades the region of positive definite matrices.}
\value{Output is a plot that is rotatable via keyboard input.  Upon exit, the latest values of the rotation parameters is
    listed to facilitate return to the latest plot}
\author{William R. Fairweather}
\examples{
\dontrun{support.p2(theta = 110, phi = 10, lseq = 150, inc = 25, 
    ticktype = "detailed")
}
}
\keyword{multivariate }
\keyword{distribution }
\keyword{hplot }
\keyword{iplot }



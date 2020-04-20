\name{maxv12}
\alias{maxv12}
\title{Rotatable Plot of Surface of Possible Maximum Values of Off-diagonal Variable}
\description{Rotatable plot of surface of possible maximum values of off-diagonal variable v12 in positive definite 
    2 x 2 matrix as a function of the diagonal variables v1 and v2.}
\usage{
maxv12(theta = 30, phi = 30, inc = 25, lseq = 200, static=FALSE, 
    ticktype="detailed", diagnose = FALSE, verbose = TRUE)
}
\arguments{
   \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{inc}{increment in degrees of plot rotations}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{static}{Logical. TRUE causes suppression of rotation}
  \item{ticktype}{simple or detailed ticks on variables}
  \item{diagnose}{Logical. TRUE causes printing of diagnostic content}
  \item{verbose}{Logical. TRUE causes printing of program ID before and after running}
}
\value{List including latest values of elements
  \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{inc}{increment in degrees of plot rotations}
  \item{ticktype}{simple or detailed ticks on variables}
}
\author{William R. Fairweather}
\examples{
maxv12(theta = 30, phi = 30, inc = 25, lseq = 200, static=TRUE, 
    ticktype = "detailed", diagnose = FALSE, verbose = TRUE)
}
\seealso{support.p2()}
\keyword{ ~multivariate }
\keyword{ ~distribution }
\keyword{ ~hplot }
\keyword{ ~iplot }

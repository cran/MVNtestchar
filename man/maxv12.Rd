\name{maxv12}
\alias{maxv12}
\title{Rotatable Plot of Surface of Possible Maximum Values of Off-diagonal Variable}
\description{Rotatable plot of surface of possible maximum values of off-diagonal variable v12 in positive definite 2 x 2 matrix}
\usage{
maxv12(theta = 30, phi = 30, inc = 25, lseq = 200, ticktype="detailed", 
    diagnose = FALSE, verbose = TRUE)
}
\arguments{
   \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{inc}{increment in degrees of plot rotations}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{ticktype}{simple or detailed ticks on variables}
  \item{diagnose}{Logical. T causes printing of diagnostic content}
  \item{verbose}{Logical. T causes printing of program ID before and after running}
}
\value{Output is a plot that is rotatable via keyboard input.  Upon exit, 
    the latest values of the rotation parameters is
    listed to facilitate return to the latest plot}
\author{William R. Fairweather}
\examples{
\dontrun{maxv12(theta = 30, phi = 30, inc = 25, lseq = 200, 
    ticktype = "detailed", diagnose = FALSE, verbose = TRUE)
}
}
\seealso{support.p2()}
\keyword{ ~multivariate }
\keyword{ ~distribution }
\keyword{ ~hplot }
\keyword{ ~iplot }

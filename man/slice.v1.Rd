\name{slice.v1}
\alias{slice.v1}
\title{Rotatable Plot of Slice Through Support Region in Positive Definite 2 x 2 Matrix}
\description{Rotatable plot of slice through support region in positive definite 2 x 2 matrix at fixed value of diagonal variable v1}
\usage{
slice.v1(level3 = 0.6, theta = 0, phi = 60, inc = 25, lseq = 100, static=FALSE, 
    ticktype="detailed", diagnose = FALSE, verbose = TRUE)
}
\arguments{
  \item{level3}{Level of V1 where slice is taken}
  \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{inc}{increment in degrees of plot rotations}
  \item{static}{Logical. TRUE causes suppression of rotation}
  \item{ticktype}{simple or detailed ticks on variables}
  \item{diagnose}{Logical. T causes printing of diagnostic content}
  \item{verbose}{Logical. T causes printing of program ID before and after running}
}
\value{List including latest values of elements
  \item{level3}{Level of V1 where slice is taken}
  \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{inc}{increment in degrees of plot rotations}
  \item{ticktype}{simple or detailed ticks on variables}
}
\author{William R. Fairweather}
\seealso{support.p2()}
\examples{
slice.v1(level3 = 0.6, theta = 0, phi = 60, inc = 25, lseq = 100, 
    static=TRUE, ticktype = "detailed")
}
\keyword{ ~multivariate }
\keyword{ ~distribution }
\keyword{ ~hplot }
\keyword{ ~iplot }




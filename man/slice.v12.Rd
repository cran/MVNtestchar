\name{slice.v12}
\alias{slice.v12}
\title{Rotatable Plot of Slice Through Support Region in Positive Definite 2 x 2 Matrix}
\description{Rotatable plot of slice through support region in positive definite 2 x 2 matrix at fixed value of off-diagonal variable v12}
\usage{
slice.v12(level3 = 0.3, theta = 30, phi = 10, inc = 25, lseq = 100, ticktype="detailed",
    diagnose = FALSE, verbose = TRUE)
}
\arguments{
  \item{level3}{Level of V1 where slice is taken}
  \item{theta}{left-right plot rotation parameter in degrees}
  \item{phi}{up-down plot rotation parameter in degrees}
  \item{inc}{increment in degrees of plot rotations}
  \item{lseq}{number of cut points in v1 and in v2}
  \item{ticktype}{simple or detailed ticks on variables}
  \item{diagnose}{Logical. T causes printing of diagnostic content}
  \item{verbose}{Logical. T causes printing of program ID before and after running}
}
\value{Output is a plot that is rotatable via keyboard input.  Upon exit, the latest values of the rotation parameters is
    listed to facilitate return to the latest plot}
\author{William R. Fairweather}
\seealso{support.p2()}
\examples{
\dontrun{slice.v12(level3 = 0.3, theta = 30, phi = 10, inc = 25, lseq = 100, 
   ticktype = "detailed") 
}
}
\keyword{ ~multivariate }
\keyword{ ~distribution }
\keyword{ ~hplot }
\keyword{ ~iplot }


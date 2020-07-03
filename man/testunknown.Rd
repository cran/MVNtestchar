\name{testunknown}
\alias{testunknown}
\title{Process the Samples Whose Distribution is to be Tested}
\description{Create positive definite matrices without nuisance parameters. 
    Tabulate distribution. Calculate goodness of fit}
\usage{testunknown(x, pvector, k, diagnose.s = FALSE, diagnose = FALSE, 
    verbose = TRUE) 
}
\arguments{
  \item{x }{Name of matrix or array.}
  \item{pvector}{Dimensionality of random vectors} 
  \item{k }{Number of cuts per unit for diagonal elements of matrix.  Program uses 2k cuts per unit for off-diagonal elements}
  \item{diagnose.s }{Logical T causes printing of diagnostic terms in internal called function(s)}
  \item{diagnose }{Logical. T causes printing of diagnostic content}
  \item{verbose }{Logical. T causes printing of function ID before and after running}
}
\value{a list including elements
  \item{Distribution}{List. Count of pd matrices within individual subcubes of pd space, 1 for each layer of list}
  \item{Goodness of fit}{List. Chi square test of goodness of fit to uniform distribution, 1 for each layer of list}
  \item{Call}{Call to testunknown function}
}  
\references{
Csorgo, M and Seshadri, V (1970). On the problem of replacing composite hypotheses by equivalent simple ones, Rev. Int. Statist. Instit., 38, 351-368
Csorgo,M and Seshadri,V (1971). Characterizing the Gaussian and exponential laws by mappings onto the unit interval, Z. Wahrscheinlickhkeitstheorie verw. Geb., 18, 333-339.
Fairweather, WR (1973). A test for multivariate normality based on a characterization. Dissertation submitted in partial fulfillment of the requirements for the Doctor of Philosophy, University of Washington, Seattle WA.
}
\author{William R. Fairweather}
\examples{
data(unknown.Np2)
testunknown(x=unknown.Np2, pvector=2, k=20, 
     diagnose.s = FALSE, diagnose = FALSE, verbose = TRUE)
}
\keyword{distribution}
\keyword{multivariate}
\keyword{array}
\keyword{math}


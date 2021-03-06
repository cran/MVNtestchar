\name{MVNtestchar-package}
\alias{MVNtestchar-package}
\alias{MVNtestchar}
\docType{package}
\title{
\packageTitle{MVNtestchar}
}
\description{
\packageDescription{MVNtestchar}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{MVNtestchar}
\packageIndices{MVNtestchar}
{Provides a test of multivariate normality of a sample which does not require estimation of the nuisance parameters, the mean      vector and covariance matrix.  Rather, a sequence of transformations removes these nuisance parameters, resulting in a set of    sample matrices that are positive definite.  If, and only if the original data is multivariate normal, these matrices are       uniformly distributed on the space of positive definite matrices in the unit hyper-rectangle.  The package performs a goodness    of fit test of this hypothesis. In addition to the test, functions in the package give visualizations of the  support region    of positive definite matrices for p equals 2.
}
}
\author{person("Fairweather", "William", email = "wrf343@flowervalleyconsulting.com",
  role = c("aut", "cre")) }
\references{
Anderson, TW. (1958), An Introduction to Multivariate Statistical Analysis, John
Wiley, New York.

Cramer, H (1962). Random Variables and Probability Distributions, Cambridge
University Press, London.

Csorgo M and Seshadri V (1970). On the problem of replacing composite hypotheses by equivalent simple ones, Rev. Int. Statist. Instit., 38, 351-368

Csorgo M and Seshadri V (1971). Characterizing the Gaussian and exponential laws by mappings onto the unit interval, Z. Wahrscheinlickhkeitstheorie verw. Geb., 18, 333-339

Deemer,WL and Olkin,I (1951). The Jacobians of certain matrix transformations
useful in multivariate analysis, *Biometrika*, **58**, 345 367.

Fairweather WR (1973). A test for multivariate normality based on a characterization. Dissertation submitted in partial fulfillment of the requirements for the Doctor of Philosophy, University of Washington, Seattle WA
}
\keyword{multivariate }
\keyword{distribution }
\keyword{hplot }
\keyword{iplot }


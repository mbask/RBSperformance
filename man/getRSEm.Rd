\name{getRSEm}
\alias{getRSEm}
\title{Relative standard error of the RBS estimates of tree proxy variable and actual variable of the same tree}
\usage{
  getRSEm(vHat, v)
}
\arguments{
  \item{vHat}{a vector of numerics holding estimates of the
  proxy variable at tree level, may hold `NA` values}

  \item{v}{actual and measured tree proxy variable}
}
\value{
  A numeric value for the relative standard error of RBS
  estimates
}
\description{
  The RSE is a measure of the precision of the RBS
  estimates of the proxy variable compared to the actual
  proxy variable. Proxy variables may be tree volume,
  biomass, C weight, and whatever else can be measurable
}
\details{
  RSEm is computed as: \deqn{RSE_m =
  \frac{\sqrt{var(\hat{V})/m}}{V}}{RSEm =
  sqrt(var(vHat)/m)/v} where m is the number of paths
  leading to m number of RBS replicates. m is computed as
  the length of the `vHat` vector, excluding `NA` values
}
\examples{
RBSestimates <- c(30,25,28,33,31)
actualVolume <- 30
getRSEm(RBSestimates, actualVolume)
# express RSEm in percent
getRSEm(RBSestimates, actualVolume) * 100
}


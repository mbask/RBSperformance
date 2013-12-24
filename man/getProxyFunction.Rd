\name{getProxyFunction}
\alias{getProxyFunction}
\title{Function to compute the proxy variable of a segment}
\usage{
  getProxyFunction(proxyVar = "kExpDiameter",
    scalingFactor = 2.5)
}
\arguments{
  \item{proxyVar}{a string defining the formula for the
  computation of the proxy variable. One of "kExpDiameter"
  (default) or "cylinder" values.}

  \item{scalingFactor}{k value, used when `proxyVar` is
  "kExpDiameter" (default 2.5)}
}
\value{
  a function to be used to compute the proxy variable at
  segment level
}
\description{
  The proxy variable, in the RBS framework, is a linearly
  well-correlated variable to the target variable and is
  used to estimate the target variable estimator
}
\note{
  This is a container function for a closure, whose
  parameters vary according to the proxy variable chosen.
  For "kExpDiameter" the function requires `d`, the
  diameter of the segment, and `l` a variable defaulting to
  `NULL` (only provided to be compatible with the
  "cylinder" function); for "cylinder" the function
  requires `d` and `l`, the length of the segment.
}
\examples{
getDProxy <- getProxyFunction()
getDProxy(d = 10)
getDHProxy <- getProxyFunction(proxyVar = "cylinder")
getDHProxy(d = 10, l = 7)
}


#' Function to compute the proxy variable of a segment
#'  
#' The proxy variable, in the RBS framework, is a linearly well-correlated variable to the target variable and is used to estimate the target variable estimator
#' 
#' @note This is a container function for a closure, whose parameters vary according to the proxy variable chosen. For "kExpDiameter" the function requires `d`, the diameter of the segment, and `l` a variable defaulting to `NULL` (only provided to be compatible with the "cylinder" function); for "cylinder" the function requires `d` and `l`, the length of the segment.
#' 
#' @param proxyVar a string defining the formula for the computation of the proxy variable. One of "kExpDiameter" (default) or "cylinder" values.
#' @param scalingFactor k value, used when `proxyVar` is "kExpDiameter" (default 2.5)
#' @return a function to be used to compute the proxy variable at segment level
#' @examples
#' getDProxy <- getProxyFunction()
#' getDProxy(d = 10)
#' getDHProxy <- getProxyFunction(proxyVar = "cylinder")
#' getDHProxy(d = 10, l = 7)
#' @export
getProxyFunction <- function(proxyVar = "kExpDiameter", scalingFactor = 2.5) {
  
  d2h <- function(d, l) {
    d^2*l
  }
  
  dk <- function(d, l = NULL) {
    d^scalingFactor
  }
  
  stopifnot(proxyVar %in% c("kExpDiameter", "cylinder"))
  
  switch(
    proxyVar
    , kExpDiameter = dk
    , cylinder     = d2h
  )   
}

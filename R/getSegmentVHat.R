#' Estimator of the total target variable of the segments belonging to a tree node, from a tree segment proxy variable
#' 
#' The target variable may be segment volume, biomass, Carbon content or whatever else
#' 
#' @references Gregoire, T G, H T Valentine, and G M Furnival. "Sampling Methods to Estimate Volume and Volume Increment." Forest Ecology and Management 21 (1987): 311-323.
#' @references Gregoire, T G, H T Valentine, and G M Furnival. "Sampling Methods to Estimate Foliage and Other Characteristics of Individual Trees" Ecology 76 (1995): 1181-1194.
#' @references Gregoire, T G, and H T Valentine. Sampling Strategies for Natural Resources and the Environment. New York: Chapmann & Hall, 2008.
#' @references Bascietto, Marco, Bruno De Cinti, Giorgio Matteucci, and Alessandro Cescatti. "Biometric Assessment of Aboveground Carbon Pools and Fluxes in Three European Forests by Randomized Branch Sampling." Forest Ecology and Management 267 (March 2012): 172-181.
#' 
#' @param targetVariable numeric positive value
#' @param probability numeric value in the range (0..1]
#' @return the estimator for the target variable at node level
#' @examples
#' getSegmentVHat(24, 0.3)
#' @export
getSegmentVHat <- function(targetVariable, probability) {
  stopifnot(targetVariable > 0 & probability > 0 & probability <= 1)
  
  targetVariable / probability
}

#' Function to estimate the volume of an intermediate segment
#'  
#' The intermediate segment is usually the a segment of the stem or of a large branch, its volume may be estimated using a selection of methods. This function computes what can be considered as the actual or true volume of an intermediate segment of a tree. The actual volume would then be used to assess the precision of RBS estimators
#' 
#' @note This is a container function for a closure, whose parameters are `areaMax`, the larger cross-section area of the segment, `areaMin`, the smaller cross-section area of the segment, and `length` the segment length.
#' 
#' @note Care must be taken to provide a common base unit of measure to `areaMax`, `areaMin`, and `length` (eg. m2, m2 and m, but not cm2, cm2 and m)
#' @param estimationMethod a string linked to the volume estimation method (unimplemented, currently default to "Smalian")
#' @return a function to be used to compute the volume of the intermediate segment
#' @examples
#' getIntermediateSegmentV <- getIntermediateVolumeFunction()
#' getIntermediateSegmentV(areaMax = pi*5^2, areaMin = pi*3^2, length = 5*100)
#' @export
getIntermediateVolumeFunction <- function(estimationMethod = "Smalian") {
  
  smalian <- function(areaMax, areaMin, length) { # Smalian's formula, aka mean section formula
    (areaMax + areaMin) / 2 * length
  }
  
  stopifnot(estimationMethod %in% c("Smalian"))

  switch(
    estimationMethod
    , Smalian = smalian
  )   
  
}

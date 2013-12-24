#' Function to estimate the volume of a final segment
#'  
#' The terminal segment is usually the final leg of a branch, its volume may be estimated using its starting cross-section area and its length, times a form coefficient. The volume may be a target variable for RBS. This function computes what can be considered as the actual or true volume of a final segment of a tree. The actual volume would then be used to assess the precision of RBS estimators
#' 
#' @note This is a container function for a closure, whose parameters are `area`, the cross-section area of the segment, and `length` the segment length.
#' 
#' @note Care must be taken to provide a common base unit of measure to `area` and `length` (eg. m2 and m, but not cm2 and m)
#' @param formCoefficient a string linked to the actual form coefficient to be used. It may take the following values: "SolidCylinder" (value = 1), "Apollonius" (default, value = 0.6), "Cubic" (value = 0.5), "SolidCone" (value = 0.33), and "Neiloid" (value = 0.25).
#' @return a function to be used to compute the volume of the terminal segment
#' @examples
#' getFinalSegmentVolume <- getFinalSegmentVolumeFunction()
#' getFinalSegmentVolume(area = pi*5^2, length = 7*100)
#' @export
getFinalSegmentVolumeFunction <- function(formCoefficient = "Apollonius") {

  formCoefficient_l = list(
    "SolidCylinder" = 1
    , "Apollonius"  = 0.6
    , "Cubic"       = 0.5
    , "SolidCone"   = 0.33
    , "Neiloid"     = 0.25
  ) 

  stopifnot(formCoefficient %in% names(formCoefficient_l))

  formCoefficientValue <- formCoefficient_l[[formCoefficient]]
  
  function(area, length) {
    area * length * formCoefficientValue
  }

}

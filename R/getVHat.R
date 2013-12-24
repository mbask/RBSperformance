#' Estimator of tree proxy variable
#' 
#' Recursively computes the estimator of the proxy variable at the tree level, from a terminal segment identified by `segmentId` in a vector of `treeSegments` ids
#' 
#' The proxy variable may be segment volume, biomass, Carbon content or whatever else
#' 
#' @note A tree segment must have 1 parent segment. The treeParentSegments vector must hold 1 NA value, corresponding to the first segment
#' @param segmentId the segment identification id
#' @param treeSegments a vector of unique segment identification ids
#' @param treeParentSegments a vector of parent identification ids; a tree segment must have a match in treeParentSegment or a NA value if the segment is the butt of the tree (ie its first segment)
#' @param pathProbabilities a vector of segment probabilities
#' @param segmentTargetVars a vector of segment proxy variables (eg volume, biomass, etc.)
#' @param vHat the estimator of the target variable at the tree level (defaults to 0, not to be set by the end-user function call)
#' @param pathProb the cumulative probability of the `segmentId` (defaults to 0, not to be set by the end-user function call)
#' @return a list of two elements, the estimator of tree proxy variable based on the `segmentId` segment, and its cumulative probability (the product of all segments individual probabilities down to the butt)
#' @export
#' @seealso \code{\link{getSegmentVHat}}
getVHat <- function(segmentId, treeSegments, treeParentSegments, pathProbabilities, segmentTargetVars, vHat = 0, pathProb = 0) {
  
  # get this segment from the vector of segments
  thisSegment <- treeSegments == segmentId
  # There must be only 1 segment
  stopifnot(sum(is.na(thisSegment), na.rm = TRUE) != 1)
  
  # get the tree segment this segment stems from (ie the parent segment)
  parentSegmentId <- as.character(treeParentSegments[thisSegment])
  # There must be 1 NA in the vector of parent segments (ie the butt segment)
  stopifnot(sum(is.na(treeParentSegments)) != 1)
  
  
  if (vHat == 0) {
    pathProb <- pathProbabilities[thisSegment]
    vHat     <- getSegmentVHat(segmentTargetVars[thisSegment], pathProb)
  }
  
  if (is.na(parentSegmentId)) { # no more segments, return estimator of tree volume, and its cumulative probability
    list(vHat, pathProb)
  } else { # add volume/probability of parent segment to vHat/pathProb
    parentSegment <- treeSegments == parentSegmentId
    
    vHat     <- vHat + getSegmentVHat(segmentTargetVars[parentSegment], pathProbabilities[parentSegment])
    pathProb <- pathProb * pathProbabilities[parentSegment]
    
    getVHat(parentSegmentId, treeSegments, treeParentSegments, pathProbabilities, segmentTargetVars, vHat, pathProb) # look for a further parent id
  }
}

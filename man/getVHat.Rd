\name{getVHat}
\alias{getVHat}
\title{Estimator of tree proxy variable}
\usage{
  getVHat(segmentId, treeSegments, treeParentSegments,
    pathProbabilities, segmentTargetVars, vHat = 0,
    pathProb = 0)
}
\arguments{
  \item{segmentId}{the segment identification id}

  \item{treeSegments}{a vector of unique segment
  identification ids}

  \item{treeParentSegments}{a vector of parent
  identification ids; a tree segment must have a match in
  treeParentSegment or a NA value if the segment is the
  butt of the tree (ie its first segment)}

  \item{pathProbabilities}{a vector of segment
  probabilities}

  \item{segmentTargetVars}{a vector of segment proxy
  variables (eg volume, biomass, etc.)}

  \item{vHat}{the estimator of the target variable at the
  tree level (defaults to 0, not to be set by the end-user
  function call)}

  \item{pathProb}{the cumulative probability of the
  `segmentId` (defaults to 0, not to be set by the end-user
  function call)}
}
\value{
  a list of two elements, the estimator of tree proxy
  variable based on the `segmentId` segment, and its
  cumulative probability (the product of all segments
  individual probabilities down to the butt)
}
\description{
  Recursively computes the estimator of the proxy variable
  at the tree level, from a terminal segment identified by
  `segmentId` in a vector of `treeSegments` ids
}
\details{
  The proxy variable may be segment volume, biomass, Carbon
  content or whatever else
}
\note{
  A tree segment must have 1 parent segment. The
  treeParentSegments vector must hold 1 NA value,
  corresponding to the first segment
}
\seealso{
  \code{\link{getSegmentVHat}}
}


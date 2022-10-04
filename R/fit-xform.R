#' Fit affine or thin plate spline transform to arbitrary transformation
#'
#' @param reg Any registration compatible with \code{nat::\link{xform}},
#'   including non-rigid and multi-step registrations. You must either supply
#'   this or the \code{refpts} argument.
#' @param samplepts A set of points in the sample (floating) space. Can be any
#'   object compatible with \code{nat::\link{xyzmatrix}}.
#' @param refpts An optional set of points in the target (fixed) space matching
#'   \code{samplepts}. You must either supply this or the \code{reg} argument.
#' @param type A character string specifying the type of registration. See
#'   \code{\link{computeTransform}} for details.
#' @param subsample A number of points to subsample from
#'   \code{samplepts,refpts}. The default value of \code{FALSE} means use all
#'   provided points to calculate the new transform.
#' @param ... Additional arguments passed to \code{\link{computeTransform}}
#'
#' @return A homogeneous affine matrix or a \code{\link{tpsreg}} object
#' @export
#'
#' @examples
#' \dontrun{
#' library(nat.flybrains)
#' reg=shortest_bridging_seq(sample='FCWB', reference="JFRC2")
#' fit_xform(reg, nat::kcs20, subsample=200, type='affine')
#' }
fit_xform <- function(reg, samplepts, refpts=NULL, type = c("affine","rigid", "similarity", "tps"), subsample=FALSE, ...) {
  if(missing(reg) && is.null(refpts))
    stop("Must supply either a registration or reference points")
  samplepts=xyzmatrix(samplepts)
  if(!is.null(refpts)) refpts=xyzmatrix(refpts)
  if(is.numeric(subsample)) {
    npts=nrow(samplepts)
    if(subsample>npts) {
      warning("subsample is > number of points! Using all of them.")
      subsample=F
    } else {
      ss=sample(npts, size = subsample)
      samplepts=samplepts[ss, , drop=F]
      if(!is.null(refpts))
        refpts=refpts[ss, , drop=F]
    }
  }
  if(is.null(refpts))
    refpts=xform(samplepts, reg)
  type=match.arg(type)
  xt=Morpho::computeTransform(refpts, samplepts, type=type, ...)
  if(type=='tps') {
    class(xt)=union('tpsreg', class(xt))
  }
  xt
}

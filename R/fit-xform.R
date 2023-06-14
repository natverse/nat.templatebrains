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
#' @param scale a 2-vector indicating the amount to scale the sample and
#'   reference points. You can supply one vector if this is the same. If the
#'   transform expects points in microns and returns points in microns then you
#'   would need \code{scale=c(1000,1)} if you want the input to be in microns
#'   and the output to be in nm.
#' @param subsample A number of points to subsample from
#'   \code{samplepts,refpts}. The default value of \code{FALSE} means use all
#'   provided points to calculate the new transform.
#' @param ... Additional arguments passed to \code{\link{computeTransform}}
#'
#' @return A homogeneous affine matrix or a \code{nat::tpsreg} object
#'   n.b. only in development nat (>= 1.10.1)
#' @export
#'
#' @examples
#' \dontrun{
#' library(nat.flybrains)
#' reg=shortest_bridging_seq(sample='FCWB', reference="JFRC2")
#' fit_xform(reg, nat::kcs20, subsample=200, type='affine')
#' # compute transform with translations in nm not microns
#' fit_xform(reg, nat::kcs20, subsample=200, type='affine', scale=1000)
#' }
#' @importFrom nat xyzmatrix xform
fit_xform <- function(reg, samplepts, refpts=NULL, type = c("affine","rigid", "similarity", "tps"), subsample=FALSE, scale=c(1, 1), ...) {
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
  if(length(scale)==1) scale=rep(scale,2)
  xt=Morpho::computeTransform(refpts*scale[2], samplepts*scale[1], type=type, ...)
  if(type=='tps') {
    class(xt)=union('tpsreg', class(xt))
  }
  xt
}


sample_points_in_surf2 <- function(bb, n, x) {
  mm=mapply(stats::runif, min=bb[1,], max=bb[2,], n = n)
  colnames(mm)=c("X","Y","Z")
  data.frame(mm, inside=pointsinside(mm,x))
}

# n defines the target number of points to find as well as the chunk size
# to use.
#' @importFrom rgl as.mesh3d
#' @importFrom nat pointsinside
sample_points_in_surf <- function(x, n){
  max_failures=100
  min_chunksize=100
  chunksize=pmax(n, min_chunksize)
  if(!inherits(x, 'mesh3d'))
    x=as.mesh3d(x)
  bb=boundingbox(x)
  nin=0
  l=list()
  # keep track of the number of times we fail to find a point to avoid
  # infinite loops
  nfails=0
  while(nin<n && nfails<max_failures) {
    df=sample_points_in_surf2(bb, chunksize, x)
    nin=nin+sum(df$inside, na.rm = T)
    if(any(df$inside))
      l[[length(l)+1]]=df[df$inside,c("X","Y","Z")]
    else nfails=nfails+1
  }
  dff=do.call(rbind, l)
  dff[seq_len(n),,drop=F]
}



#' Fit a single transform to a bridging registration between brains
#'
#' @param pts Optional set of points to use for the fit. If they are not
#'   specified they will be randomly sampled (see details).
#' @param npts Number of points to use for fit (defaults to 1000 when no
#'   \code{pts} argument is specified).
#' @param ... Additional arguments passed to \code{\link{fit_xform}}
#' @inheritParams xform_brain
#' @inheritParams fit_xform
#'
#' @details If \code{pts} are supplied these will be used to construct the fit.
#'   This might be useful if you are focussing on a particular brain region and
#'   supply relevant neurons. But if you want to work with one registration to
#'   use for the whole brain then it's better to use points across the brain.
#'
#'   When points are not provided then \code{fit_xform_brain} will first check
#'   to see if it can find a neuropil surface model for the \code{sample}. If it
#'   can, it will sample a number of points that lie inside the surface model.
#'   If no surface model can be found, then it will look for a
#'   \code{\link{templatebrain}} object specifying a bounding box around the
#'   sample brain. If successful, it will sample \code{npts} within that
#'   bounding box. If not there will be an error and you will have to supply the
#'   points.
#'
#' @return A homogeneous affine matrix or a \code{nat::tpsreg} object
#'   n.b. only in development nat (>= 1.10.1)
#' @seealso \code{\link{fit_xform}}
#' @export
#'
#' @examples
#' \dontrun{
#' library(nat.flybrains)
#' t1=fit_xform_brain(sample='FCWB', reference="JFRC2", type='affine')
#' t1
#' # the same but for points in nm
#' fit_xform_brain(sample='FCWB', reference="JFRC2", type='affine', scale=1000)
#' # run the fit based on a particular group of Kenyon cells
#' t2=fit_xform_brain(sample='FCWB', reference="JFRC2",
#'   pts=nat::kcs20, npts=300, type='affine')

#'
#' nclear3d()
#' mfrow3d(1, 2, sharedMouse = TRUE)
#' plot3d(JFRC2)
#' plot3d(xform(FCWB.surf, t1))
#' next3d()
#' plot3d(JFRC2)
#' plot3d(xform(FCWB.surf, t2))
#' # the whole brain surfaces clearly match better when you fit on the whole brain
#' }
fit_xform_brain <- function(sample, reference, via=NULL,
                            type=c("affine","rigid", "similarity", "tps"),
                            pts=NULL,
                            npts=if(is.null(pts)) 1000 else NULL,
                            scale=c(1,1),
                            ...) {
  sbs=shortest_bridging_seq(sample = sample, reference = reference, via=via)
  if(is.null(pts)) {
    if(!is.templatebrain(sample))
      sample <- get_templatebrain(as.character(sample))
    # find surface
    atb=all_templatebrains()
    m=match(as.character(sample), atb$name)
    if(!is.null(sample) && !is.na(m)) {
      b = try(get(paste0(atb$name[m], '.surf'), mode = 'list'), silent = T)
      if(inherits(b, c('hxsurf', 'mesh3d'))) {
        if(interactive())
          message("sampling points within surface model for:", as.character(sample))
        b=as.mesh3d(b)
      }
      else {
        if(interactive())
          message("sampling points within template brain bounding box for:", as.character(sample))
        b=as.mesh3d(boundingbox(sample))
      }
      pts=sample_points_in_surf(b, npts)
    } else stop("Failed to find template brain!")
  }

  fit_xform(reg = sbs, samplepts = pts, type=type, subsample = ifelse(is.null(npts), F, npts), scale=scale, ...)
}

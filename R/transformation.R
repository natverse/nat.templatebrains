brain2reg <- function(reference, sample, mirror=FALSE) {
  if(mirror) {
    stem=paste0(reference$regName, "_mirror.list")
    extradirs=getOption('nat.flybrains.extramirror')
    if(!is.null(extradirs)) {
      for(extradir in extradirs){
        reg <- file.path(extradir,stem)
        if(file.exists(reg)) return(reg)
      }
    }
    reg <- extdata(file.path("mirroringregistrations", stem))
  } else {
    stem=paste0(reference$regName, "_", sample$regName, ".list")
    extradirs=getOption('nat.flybrains.extrabridge')
    if(!is.null(extradirs)) {
      for(extradir in extradirs){
        reg <- file.path(extradir,stem)
        if(file.exists(reg)) return(reg)
      }
    }
    reg <- extdata(file.path("bridgingregistrations",stem))
  }
  reg
}

#' Transform 3D object between template brains
#'
#' @details NB the sample and reference brains can either be
#'   \code{templatebrain} objects or a character string containing the short
#'   name of the template e.g. \code{"IS2"}.
#' @param x the 3D object to be transformed
#' @param sample source template brain (e.g. IS2) that data is currently in.
#' @param reference target template brain (e.g. IS2) that data should be
#'   transformed into.
#' @param ... extra arguments to pass to \code{\link[nat]{xform}}.
#' @export
xform_brain <- function(x, sample, reference, ...) {
  direction <- 'inverse'
  if(is.character(reference)) reference=templatebrain(name=reference)
  if(!missing(sample) && is.character(sample)) sample=templatebrain(name=sample)
  reg <- brain2reg(reference, sample)
  if(reg == "") {
    reg <- brain2reg(sample, reference)
    if(reg == "") stop("No suitable registration found.")
    message("Numerically inverting registration from ", reference$regName,
            " to ", sample$regName,
            ". This may take some time and results may be inaccurate.")
    direction <- 'forward'
  }
  nat::xform(x, reg=reg, direction=direction, ...)
}

#' Mirror 3D object around a given axis, optionally using a warping registration
#'
#' @param x the 3D object to be mirrored.
#' @param brain source template brain (e.g. IS2) that data is in.
#' @param mirrorAxis the axis to mirror (default \code{"X"}).
#' @param ... extra arguments to pass to \code{\link[nat]{xform}}.
#' @export
mirror_brain <- function(x, brain, mirrorAxis=c("X","Y","Z"), ...) {
  warpfile <- brain2reg(reference=brain, mirror=TRUE)
  mirrorAxis <- match.arg(mirrorAxis)
  axisCol <- which(mirrorAxis == c("X", "Y", "Z"))
  mirrorAxisSize <- brain$BoundingBox[2, axisCol] - brain$BoundingBox[1, axisCol]
  nat::mirror(x, mirrorAxisSize=mirrorAxisSize, mirrorAxis=mirrorAxis, warpfile=warpfile, ...)
}

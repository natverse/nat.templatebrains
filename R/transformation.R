# return path to mirroring registration for a template brain
mirror_reg<-function(brain, ...) {
  regname=paste0(as.character(brain), "_mirror.list")
  find_reg(regname, ...)
}

bridging_sequence<-function(reference, sample, via=NULL, ...) {
  if(!is.null(via)) {
    if(is.templatebrain(via)) via=list(via)
    via=sapply(via, as.character, USE.NAMES = F)
  }
  # TODO check this order carefully, especially with multiple via brains
  all_brains=c(as.character(reference), via, as.character(sample))
  Map(function(r, s, ...) bridging_reg(reference=r, sample=s, ...),
      r=all_brains[-length(all_brains)],
      s=all_brains[-1],
      ...)
}

# return path to bridging registration between template brains
bridging_reg <- function(reference, sample, checkboth=FALSE, mustWork=FALSE) {
  reference=as.character(reference)
  sample=as.character(sample)
  regname=paste0(reference, "_", sample, ".list")
  tryCatch(
    if(checkboth){
      reg=find_reg(regname, mustWork=FALSE)
      if(reg==""){
        # try again, marking the registration as swapped
        regname=paste0(sample, "_", reference, ".list")
        structure(find_reg(regname, mustWork=TRUE), swapped=TRUE)
      } else reg
    } else {
      find_reg(regname, mustWork=mustWork)
    },
    error=function(e) stop("Unable to find bridging registration between ",
                           reference, " and ", sample)
  )
}

# find a registration checking a vector of extradirs and then defaultreldir
find_reg<-function(regname, regdirs=getOption('nat.templatebrains.regdirs'), mustWork=FALSE) {
  if(is.null(regdirs)) {
    stop("No registration directories set. See options section of ?nat.templatebrains")
  } else {
    for(regdir in regdirs){
      reg <- file.path(regdir,regname)
      if(file.exists(reg)) return(reg)
    }
  }
  if(mustWork) stop("Unable to find registration: ", regname, ' in folders: ',
                    paste(regdirs, collapse="\n"))
  ""
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
#' @param via optional intermediate brain to use when there is no direct
#'   bridging registration.
#' @param ... extra arguments to pass to \code{\link[nat]{xform}}.
#' @export
xform_brain <- function(x, sample, reference, via=NULL, ...) {
  if(!is.null(via)){
    if(!is.templatebrain(via) && length(via)>1)
      stop("Currently only support for one intermediate brain")
    x = xform_brain(x, sample=sample, reference=via, ...)
    return(xform_brain(x, sample=via, reference=reference, ...))
  }
  reg <- bridging_reg(reference, sample, checkboth = T, mustWork = T)
  direction <- ifelse(isTRUE(attr(reg,'swapped')), 'forward', 'inverse')
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
  warpfile <- mirror_reg(brain)
  mirrorAxis <- match.arg(mirrorAxis)
  axisCol <- which(mirrorAxis == c("X", "Y", "Z"))
  mirrorAxisSize <- brain$BoundingBox[2, axisCol] - brain$BoundingBox[1, axisCol]
  nat::mirror(x, mirrorAxisSize=mirrorAxisSize, mirrorAxis=mirrorAxis, warpfile=warpfile, ...)
}

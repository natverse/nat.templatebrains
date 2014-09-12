# return path to mirroring registration for a template brain
mirror_reg<-function(brain, ...) {
  regname=paste0(as.character(brain), "_mirror.list")
  find_reg(regname, ...)
}

#' Find sequence of one or more bridging registrations
#'
#' @description This function is primarily intended for developer use (it is
#'   used inside \code{xform_brain}) but may be useful for end users.
#'
#' @details When \code{checkboth=FALSE}, only registrations that can be directly
#'   used to map image data from sample to reference are returned. When working
#'   with 3D points, use \code{checkboth=TRUE}. Note that all possible
#'   directories will first be scanned for registrations in the preferred
#'   direction and then rescanned for the opposite direction if nothing is
#'   found.
#'
#' @section registration direction: When mapping points from JFRC2 -> IS2 -> FCWB
#'   (i.e. sample=JFRC2, via=IS2, ref=FCWB) the command line passed to CMTK's streamxform
#'   should look like:
#' \verb{streamxform -- JFRC2_IS2.list --inverse FCWB_IS2.list}
#' However when mapping image data
#' the command line for CMTK's reformatx should look like:
#' \verb{reformatx -- JFRC2_IS2.list --inverse FCWB_IS2.list}
#' and the corresponding output might look like \verb{
#' list(JFRC2 = structure(
#'        "/GD/dev/R/nat.flybrains/inst/extdata/bridgingregistrations/JFRC2_IS2.list",
#'        swapped = TRUE),
#'      IS2 = "/GD/dev/R/nat.flybrains/inst/extdata/bridgingregistrations/FCWB_IS2.list")
#' }
#' @inheritParams xform_brain
#' @param checkboth Whether to look for registrations in both directions. The
#'   default (\code{checkboth=FALSE}) will only return registrations in the
#'   forward direction (see details).
#' @param mustWork Whether to error out if appropriate registrations are not
#'   found.
#' @export
#' @examples
#' \dontrun{
#' bridging_sequence(sample=JFRC2, ref=FCWB, checkboth = T)
#' bridging_sequence(sample=JFRC2, via=IS2, ref=FCWB, checkboth = T)
#' }
bridging_sequence<-function(sample, reference, via=NULL, checkboth=FALSE,
                            mustWork=FALSE) {
  if(!is.null(via)) {
    if(is.templatebrain(via)) via=list(via)
    via=sapply(via, as.character, USE.NAMES = F)
  }
  # TODO check this order carefully, especially with multiple via brains
  all_brains=c(as.character(sample), via, as.character(reference))
  mapply(bridging_reg,
         sample=all_brains[-length(all_brains)],
         reference=all_brains[-1],
         MoreArgs = list(checkboth=checkboth, mustWork=mustWork),
         SIMPLIFY = FALSE)
}

# return path to bridging registration between template brains
bridging_reg <- function(sample, reference, checkboth=FALSE, mustWork=FALSE) {
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
  regs <- bridging_sequence(reference=reference, sample=sample, via=via,
                            checkboth = T, mustWork = T)
  directions <- sapply(regs, function(reg)
    ifelse(isTRUE(attr(reg,'swapped')), 'forward', 'inverse'))
  nat::xform(x, reg=as.character(regs), direction=directions, ...)
}

#' Mirror 3D object around a given axis, optionally using a warping registration
#'
#' @param x the 3D object to be mirrored.
#' @param brain source template brain (e.g. IS2) that data is in.
#' @param mirrorAxis the axis to mirror (default \code{"X"}).
#' @param transform whether to use warp (default) or affine component of
#'   registration, or simply flip about midplane of axis.
#' @param ... extra arguments to pass to \code{\link[nat]{mirror}}.
#' @export
#' @examples
#' data(FCWB.demo)
#' data(kcs20, package='nat')
#' # Simple mirror along the x i.e. medio-lateral axis
#' kcs20.flip=mirror_brain(kcs20, FCWB.demo, transform='flip')
#' ## full non-rigid mirroring to account for differences in shape/centering of
#' ## template brain. Depends on nat.flybrains package and system CMTK installation
#' \dontrun{
#' kcs20.right=mirror_brain(kcs20, FCWB, transform='flip')
#' plot3d(kcs20, col='red')
#' plot3d(kcs20.right, col='green')
#' # include surface plot of brain
#' plot3d(FCWB)
#' }
mirror_brain <- function(x, brain, mirrorAxis=c("X","Y","Z"),
                         transform = c("warp", "affine", "flip"), ...) {
  transform=match.arg(transform)
  warpfile <- if(transform=="flip") NULL else mirror_reg(brain)
  mirrorAxis <- match.arg(mirrorAxis)
  axisCol <- which(mirrorAxis == c("X", "Y", "Z"))
  mirrorAxisSize <- brain$BoundingBox[2, axisCol] - brain$BoundingBox[1, axisCol]
  nat::mirror(x, mirrorAxisSize=mirrorAxisSize, mirrorAxis=mirrorAxis, warpfile=warpfile, ...)
}

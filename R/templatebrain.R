#' Construct templatebrain object for an image registration template
#'
#' \code{templatebrain} objects encapsulate key information for the reference
#' brain in an image registration. Usually this will be a standard template
#' brain used for many registrations. \strong{It will normally be much more
#' convenient to use  \code{\link{as.templatebrain}} methods to convert an image
#' file or an im3d object into a \code{templatebrain}}.
#'
#' \code{templatebrain} objects are only useful for transformation processes
#' when the \code{BoundingBox} is specified to define the physical extent of the
#' volume. We use the definition of the Amira 3D visualisation and analysis
#' software. This corresponds to the \strong{node} centers option in the
#' \href{http://teem.sourceforge.net/nrrd/format.html}{NRRD format}. The
#' bounding box can be obtained from nrrd or amiramesh format files.
#'
#' @param name the name of the template.
#' @param regName the short name to use for finding appropriate registrations.
#' @param type one of \code{c('single brain', 'average')}, indicating whether
#'   the template brain has been created from just one image, or is the average
#'   of multiple images.
#' @param sex the sex of the template brain. For templates with
#'   \code{type=='average'}, the possibility of \code{sex='intersex'} exists.
#' @param dims dimensions of the image (number of voxels)
#' @param BoundingBox physical dimensions of the image (see
#'   \code{\link[nat]{boundingbox}})
#' @param voxdims physical spacing between voxels
#' @param units units of physical measurements (e.g. microns)
#' @param description details of the template.
#' @rdname templatebrain
#' @param ... additional named arguments that will be added as fields to the
#'   \code{templatebrain} object
#' @return A list with class \code{templatebrain}.
#' @export
#' @seealso \code{\link{as.templatebrain}}, \code{\link[nat]{im3d},
#'   \link[nat]{boundingbox}}
templatebrain<-function(name, regName=name, type=NULL, sex=NULL, dims=NULL,
                        BoundingBox=NULL, voxdims=NULL, units=NULL,
                        description=NULL, ...) {
  template <- structure(list(name=name, regName=name, type=type, sex=sex,
                             dims=dims, voxdims=voxdims, origin=origin,
                             BoundingBox=BoundingBox, units=units,
                             description=description),
                        class="templatebrain")
  if(!missing(...)) {
    apl=pairlist(...)
    template[names(apl)]=apl
  }
  template
}

#' Use image file or other object to initialise template brain
#'
#' @param x object used to construct the templatebrain, either a character
#'   vector with the path to a file or a \code{im3d} object.
#' @param ... additional named arguments passed to methods or to
#'   \code{templatebrain} and that will be added as fields to the
#'   \code{templatebrain} object.
#' @return A list with class \code{templatebrain}.
#' @export
#' @seealso  \code{\link[nat]{im3d}}
#' @rdname as.templatebrain
as.templatebrain <- function(x, ...) UseMethod("as.templatebrain")

#' @rdname as.templatebrain
#' @param name name of the template brain. Will use the filename (minus final
#'   extension) by default for \code{as.templatebrain.character} but must be
#'   supplied for \code{as.templatebrain.im3d}.
#' @importFrom nat read.im3d voxdims boundingbox origin
#' @export
as.templatebrain.character <- function(x, name=NULL, ...) {
  if(!file.exists(x)) stop("x does not specify a valid path!")
  im3d <- read.im3d(x, ReadData=FALSE)
  if(is.null(name)) sub("\\.[^.]+$", "", basename(x))
  templatebrain(im3d, name=name, ...)
}

#' @rdname as.templatebrain
#' @export
as.templatebrain.im3d <- function(x, name, ...) {
  # This will be incorrect if the directions are not rectilinear
  units <- attr(x, 'header')$'space units'
  templatebrain(name=name, dims=dim(im3d), voxdims=voxdims(im3d),
                origin=origin(im3d), BoundingBox=boundingbox(im3d),
                units=units, ...)
}

#' Test if object is templatebrain
#' @param x Object to test
#' @return logical
is.templatebrain<-function(x) inherits(x, 'templatebrain')

#' Convert template brain to character vector representation
#'
#' This will normally be used to extract the short name i.e. \code{regName}.
#' @param x templatebrain to convert
#' @param field Which field to use (defaults to \code{'regName'})
#' @param ... additional arguments (currently ignored)
#' @return character vector
#' @export
as.character.templatebrain<-function(x, field=c('regName','name'), ...){
  field=match.arg(field)
  x[[field]]
}

#' Print brain template information in human-readable form
#'
#' @param x the object of class \code{templatebrain} to print.
#' @param ... further objects to print.
#' @export
print.templatebrain <- function(x, ...) {
  cat("=== Template Brain ===", "\n")
  cat("Name:", x$name, "\n")
  cat("Type:", x$type, "\n")
  cat("Sex: ", x$sex, "\n")
  cat(paste0("Voxel size:\n"))
  cat("  x =", paste0(x$voxdims[1], " ", x$units[1], "\n"))
  cat("  y =", paste0(x$voxdims[2], " ", x$units[2], "\n"))
  cat("  z =", paste0(x$voxdims[3], " ", x$units[3], "\n"))
  cat(paste0("Bounding box (", x$units[1], "):\n"))
  cat("  x =", paste0(x$BoundingBox[1, 1], ", y = ", x$BoundingBox[1, 2], ", z = ", x$BoundingBox[1, 3], ",\n"))
  cat("  x =", paste0(x$BoundingBox[2, 1], ", y = ", x$BoundingBox[2, 2], ", z = ", x$BoundingBox[2, 3], ".\n"))
  cat("Description:", x$description)
  if(exists('...')) {
    cat("\n")
    cat(...)
  }
}

#' @export
#' @method as.im3d templatebrain
#' @importFrom nat as.im3d
as.im3d.templatebrain <- function(x, ...) {
  newim3d <- nat::im3d(NA, dims=x$dims, voxdims=x$voxdims, origin=x$origin)
  newim3d
}

#' @export
#' @method origin templatebrain
#' @importFrom nat origin
origin.templatebrain <- function(x, ...) {
  origin(nat::as.im3d(x))
}

#' @export
#' @method dim templatebrain
dim.templatebrain <- function(x, ...) {
  dim(nat::as.im3d(x))
}

#' @export
#' @method voxdims templatebrain
#' @importFrom nat voxdims
voxdims.templatebrain <- function(x, ...) {
  voxdims(nat::as.im3d(x))
}

#' @export
#' @method boundingbox templatebrain
#' @importFrom nat boundingbox
boundingbox.templatebrain <- function(x, ...) {
  boundingbox(nat::as.im3d(x))
}

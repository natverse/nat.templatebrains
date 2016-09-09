#' Get or set the registration template space in which an object lives
#'
#' @details In order to facilitate transformations between objects in defined
#'   anatomical spaces these functions allow the registration template for an
#'   object to be specified. Most of the time you will not need to use these
#'   functions manually since the appropriate space will be set by the function
#'   xform_brain and friends.
#'
#' @param x The 3D object whose registration space will be set/returned
#'
#' @return Either a \code{templatebrain} object or the newly tagged object
#' @export
#'
#' @examples
#' \dontrun{
#' library(nat.flybrains)
#' kcs3=kcs20[1:3]
#' regtemplate(kcs3)=FCWB
#' regtemplate(kcs3)
#'
#' kcs3m=mirror_brain(kcs3, brain=regtemplate(kcs20))
#' plot3d(kcs3, col='red')
#' plot3d(kcs3m, col='green')
#' }
regtemplate <- function(x) {
  get_templatebrain(attr(x, 'regtemplate'))
}


#' @param value The registration template brain (either a character vector
#'   naming the space or a \code{\link{templatebrain}} object)
#' @rdname regtemplate
#' @export
`regtemplate<-` <- function(x, value) {
  attr(x, 'regtemplate') <- get_templatebrain(value)
  x
}

# TODO fancier way of finding brains that avoids the possibility of brains in
# package being aliased by objects of the same name earlier in the search path
# (e.g. in the Global environment)
# when strict is FALSE we will return NULL if we can't find an actual template
# brain - otherwise we could think about tagging with a character vector
get_templatebrain <- function(x, strict=FALSE) {
  if(is.templatebrain(x) || is.null(x)) return(x)
  b = try(get(x, mode = 'list'), silent = T)
  if (!is.templatebrain(b) && strict)
    stop("Unable to find template brain: ", b)
  if(inherits(b, 'try-error')) NULL else b
}

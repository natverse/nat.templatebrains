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

all_templatebrains_m <- memoise::memoise(function() {
  ll=apropos(what='.*', mode='list', where=TRUE)
  df=data.frame(object=ll, pos=as.integer(names(ll)),
                stringsAsFactors = FALSE)

  brain_details <- function(x, pos) {
    obj=get(x, pos = pos)
    env.name=attr(as.environment(pos), 'name')
    if(is.null(env.name)) env.name=NA_character_
    env.name=sub("package:","", env.name)
    if(is.templatebrain(obj)) {
      dims=obj[['dims']]
      name=as.character(obj)
    } else {
      dims=rep(NA_integer_, 3L)
      name=NA_character_
    }
    data.frame(package=env.name, name=name, md5=digest::digest(obj, algo = 'md5'),
               W=dims[1],H=dims[2],D=dims[3], stringsAsFactors = FALSE)
  }

  details <- do.call(rbind, mapply(brain_details, df$object, df$pos, SIMPLIFY = FALSE))
  df=cbind(df, details)
  df=df[!is.na(df$name),]
  rownames(df)=NULL
  df
})


#' @description \code{all_templatebrains} returns a data.frame detailing all
#'   \code{templatebrain} objects on the search path (including those inside
#'   packages).
#'
#' @param remove.duplicates Whether to remove duplicate template brains (as
#'   determined by md5 hash) from the result list
#' @return For \code{all_templatebrains}, a \code{data.frame} containing the
#'   following columns: \itemize{
#'
#'   \item object The name of the \code{templatebrain} object
#'
#'   \item pos An integer specifying the environment
#'
#'   \item package Character vector naming the environment
#'
#'   \item md5 md5 hash of the \code{templatebrain} object
#'
#'   \item name
#'
#'   \item W,H,D Width, height and depth of image stack (pixels)
#'
#'   }
#' @export
#' @rdname guess_templatebrain
all_templatebrains <- function(cached=TRUE, remove.duplicates=FALSE) {
  if(isFALSE(cached))
    memoise::forget(all_templatebrains_m)
  res=all_templatebrains_m()
  if(remove.duplicates)
    res[!duplicated(res[['md5']]),,drop=FALSE]
  else res
}


#' Find all template brains or those matching a given image volume
#'
#' @param x An image object
#' @param rval Whether to return the \code{\link{templatebrain}} object itself
#'   or just its name.
#' @param cached When \code{TRUE} returns precomputed (memoised) results,
#'   otherwise rescans searching for all template brains.
#' @param mustWork Whether to insist that exactly one template brain is found
#'
#' @return \code{guess_templatebrain} returns a \code{\link{templatebrain}}
#'   object when \code{rval='templatebrain'} or a character vector when
#'   \code{rval='name'}.
#' @export
#'
#' @seealso \code{\link{templatebrain}}
#' @examples
#' \dontrun{
#' all_templatebrains()
#'
#' guess_templatebrain(im3d(dims=c(30,40,50)))
#' guess_templatebrain('path/to/my/image.nrrd')
#'
#' if(require('nat.flybrains')){
#'   guess_templatebrain(im3d(dims=c(1024,512,218)), rval = 'name')
#'   # get the matching template brain
#'   tb=guess_templatebrain(im3d(dims=c(1024,512,218)), rval = 'brain')
#'   # get its voxel dimensions
#'   voxdims(tb)
#' }
#' }
guess_templatebrain <- function(x, rval=c("templatebrain", "name"),
                                cached=TRUE, mustWork=FALSE) {
  tx=as.templatebrain(x, regName='dummy')
  rval=match.arg(rval)
  df=all_templatebrains(cached = cached, remove.duplicates = TRUE)
  df$dims=apply(df[c("W","H","D")],1,paste, collapse="x")
  dims=paste(tx$dims, collapse="x")
  candidates=df[dims==df$dims,,drop=FALSE]
  if(nrow(candidates)>1) {
    if(mustWork) {
      print(candidates)
      stop("Multiple candidates!")
    }
  }
  if(nrow(candidates)==0)
    if(mustWork) stop("No candidates found!")

  if(rval=='name') {
    # unique because we will sometimes have aliases
    unique(candidates$name)
  } else {
    if(nrow(candidates)==0) return(NULL)
    if(nrow(candidates)>1)
      mapply(get, x=candidates$object, pos=candidates$pos, SIMPLIFY = FALSE)
    else get(candidates$object, pos = candidates$pos)
  }
}

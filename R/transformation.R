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
#' @section Registration direction: When mapping points from JFRC2 -> IS2 -> FCWB
#'   (i.e. sample=JFRC2, via=IS2, ref=FCWB) the command line passed to CMTK's streamxform
#'   should look like:
#' \verb{streamxform -- JFRC2_IS2.list --inverse FCWB_IS2.list}
#' However when mapping image data
#' the command line for CMTK's reformatx should look like:
#' \verb{reformatx  -o out.nrrd --floating JFRC2.nrrd FCWB.nrrd FCWB_IS2.list --inverse JFRC2_IS2.list}
#' \code{bridging_sequence} produces output like \verb{
#' list(JFRC2 = structure(
#'        "/GD/dev/R/nat.flybrains/inst/extdata/bridgingregistrations/JFRC2_IS2.list",
#'        swap = TRUE),
#'      IS2 = "/GD/dev/R/nat.flybrains/inst/extdata/bridgingregistrations/FCWB_IS2.list")
#' }
#' in these circumstances, which xformpoints.cmtkreg turns into "-- JFRC2_IS2.list --inverse FCWB_IS2.list".
#' @inheritParams xform_brain
#' @param checkboth whether to look for registrations in both
#'   directions. The default (\code{checkboth=FALSE}) will only return
#'   registrations in the forward direction (see details).
#' @param mustWork whether to error out if appropriate registrations are not
#'   found.
#' @export
#' @examples
#' \dontrun{
#' bridging_sequence(sample=JFRC2, ref=FCWB, checkboth = T)
#' bridging_sequence(sample=JFRC2, via=IS2, ref=FCWB, checkboth = T)
#' }
bridging_sequence<-function(sample, reference, via=NULL, imagedata=FALSE,
                            checkboth=!imagedata, mustWork=FALSE) {
  if(!is.null(via)) {
    if(is.templatebrain(via)) via=list(via)
    via=sapply(via, as.character, USE.NAMES = F)
  }
  # TODO check this order carefully, especially with multiple via brains
  all_brains=c(as.character(sample), via, as.character(reference))
  if(length(unique(all_brains))==1)
    return(NULL) # nothing to do
  seq=mapply(bridging_reg,
         sample=all_brains[-length(all_brains)],
         reference=all_brains[-1],
         MoreArgs = list(checkboth=checkboth, mustWork=mustWork),
         SIMPLIFY = FALSE)
  simplify_bridging_sequence(seq)
}

# convert to well behaved nat::reglist
simplify_bridging_sequence<-function(x) {
  if(!is.list(x)) stop("simplify_bridging_sequence expects a list!")
  # convert all elements of x to reglists ...
  make_reglist <- function(r) {
    swap=NULL
    if(is.character(r) && tools::file_ext(r)=="rds"){
      swap=attr(r, 'swap')
      r=readRDS(r)
    }
    rl=nat::reglist(r)
    if(isTRUE(swap)) rl=nat::invert_reglist(rl)
    rl
  }
  rlx = lapply(x, make_reglist)
  # and then join them together into a single reglist
  do.call(c, rlx)
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
        structure(find_reg(regname, mustWork=TRUE), swap=TRUE)
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

#' Make data.frame with details of all registrations
#'
#' @param regdirs Character vector of directories to search for registrations
#'   (see details)
#' @details by default \code{regdirs} is set to
#'   getOption('nat.templatebrains.regdirs')
#' @return data.frame with one row for each observed registration and columns
#'   \itemize{
#'
#'   \item path
#'
#'   \item name
#'
#'   \item dup
#'
#'   \item bridge
#'
#'   \item reference
#'
#'   \item sample }
#'
#'   If there are no registrations, there will be a data.frame with 0 rows and
#'   these columns.
#' @export
#' @examples
#' \dontrun{
#' allreg_dataframe()
#' }
allreg_dataframe<-function(regdirs=getOption('nat.templatebrains.regdirs')) {
  if(!length(regdirs)) regdirs=character()
  df=data.frame(path=dir(regdirs, pattern = '\\.(list|rds)$', full.names = T),
                stringsAsFactors = F)
  df$name=basename(df$path)
  df$dup=duplicated(df$name)
  df$bridge=!grepl("(mirror|imgflip)\\.list",df$name)
  df$reference=gsub("^([^_]+)_.*","\\1", df$name)
  df$sample=gsub("^[^_]+_([^.]+).*","\\1", df$name)
  # set mirroring registration sample brain to NA
  df$sample[!df$bridge]=NA_character_
  df
}

#' Make or query connected graph of bridging registrations
#'
#' @description These functions are designed for expert use. In general it is
#'   recommended to use \code{xform_brain}.
#'
#'   \code{bridging_graph} creates an igraph::graph representing all known
#'   template brains (vertices) and the bridging registrations connecting them
#'   (edges).
#' @details When \code{reciprocal != NA} we create a graph where each forward
#'   transformation is matched by a corresponding inverse transformation with
#'   the specified edge weight. The edge weight for forward transforms will
#'   always be 1.0.
#'
#'   By default \code{regdirs} is set to getOption('nat.templatebrains.regdirs')
#' @rdname shortest_bridging_seq
#' @param reciprocal Sets the weight of reciprocal edges in the graph (and
#'   thereby whether inverse registrations will be considered).
#' @inheritParams allreg_dataframe
#' @seealso \code{\link{allreg_dataframe}}
#' @export
#' @importFrom igraph E E<- graph.edgelist
#' @examples
#' \dontrun{
#' plot(bridging_graph(reciprocal=3), vertex.size=25)
#' # the same including
#' plot(bridging_graph(), vertex.size=25)
#' }
bridging_graph <- function(regdirs=getOption('nat.templatebrains.regdirs'), reciprocal=NA) {
  sha512=digest(list(regdirs, reciprocal), algo = "sha512")
  bg=.bridging_graph[[sha512]]
  if(is.null(bg)){
    bg=make_bridging_graph(regdirs, reciprocal)
    .bridging_graph[[sha512]]=bg
  }
  bg
}

set_bridging_graph <- function(bg, regdirs=getOption('nat.templatebrains.regdirs'), reciprocal=NA) {
  sha512=digest(list(regdirs, reciprocal), algo = "sha512")
  .bridging_graph[[sha512]]=bg
}

make_bridging_graph <- function(regdirs, reciprocal) {
  df=allreg_dataframe(regdirs)
  if(nrow(df)==0) return(NULL)
  # just keep the bridging registrations
  df=df[df$bridge & !df$dup,]
  el=as.matrix(df[,c("sample","reference")])
  if(is.na(reciprocal)){
    g=graph.edgelist(el, directed = T)
    E(g)$path=df$path
    E(g)$swap=FALSE
  } else {
    # make reciprocal edges
    el2=rbind(el,el[,2:1])
    g=igraph::graph.edgelist(el2, directed = T)
    E(g)$weight=rep(c(1, reciprocal), rep(nrow(el), 2))
    E(g)$swap=rep(c(FALSE, TRUE), rep(nrow(el), 2))
    E(g)$path=c(df$path,df$path)
  }
  g
}





#' @importFrom memoise forget memoise
reset_cache <- function() {
  # memoise::forget(shortest_bridging_seq)
  # memoise::memoise(shortest_bridging_seq)
  rm(list=ls(.bridging_graph), envir = .bridging_graph)
}

#' @description \code{shortest_bridging_seq} finds the shortest bridging
#'   sequence on a graph of all available bridging registrations, subject to
#'   constraints defined by graph connectivity and the \code{reciprocal
#'   parameter}.
#' @importFrom igraph shortest.paths get.shortest.paths E vertex_attr
#' @export
#' @param ... additional arguments passed on to \code{\link{bridging_graph}}
#' @inheritParams xform_brain
#' @examples
#' \dontrun{
#' shortest_bridging_seq(FCWB, IS2)
#' # or
#' shortest_bridging_seq('FCWB', 'IS2')
#' }
shortest_bridging_seq <-
  function(sample, reference, checkboth = TRUE, imagedata = FALSE, reciprocal=NA, ...) {
    reciprocal <- if (checkboth && is.na(reciprocal)) {
      ifelse(imagedata, 100, 1.01)
    } else NA

    sample = as.character(sample)
    reference = as.character(reference)

    # nothing to do ...
    if(isTRUE(all.equal(sample, reference, check.attributes=FALSE)))
      return(NULL)

    g = bridging_graph(reciprocal = reciprocal, ...)
    if(is.null(g)) stop("No bridging registrations available!")
    vertex_names <- vertex_attr(g, "name")
    if (!sample %in% vertex_names)
      stop("Sample template: ", sample,
           " has no known bridging registrations!")
    if (!reference %in% vertex_names)
      stop("Reference template: ", reference,
           " has no known bridging registrations!")

    # treat as directed
    sp = shortest.paths(g, v = sample, to = reference, mode = 'out')
    if (!is.finite(sp))
      stop("No path between: ", reference, " and ", sample, "!")

    if (sp > 100)
      warning("Bridging seq requires an inversion. This is very slow for image data!")

    gsp = get.shortest.paths(
      g, from = sample, to = reference, mode = 'out', output = 'epath'
    )
    epath = gsp$epath[[1]]
    seq=mapply(function(x,y) {
      if (y)
        attr(x,'swap') = y;x
    },
    E(g)[epath]$path, E(g)[epath]$swap,
    USE.NAMES = F, SIMPLIFY = F)
    simplify_bridging_sequence(seq)
  }

#' Transform 3D object between template brains
#'
#' @details NB the \code{sample}, \code{reference} and \code{via} brains can
#'   either be \code{templatebrain} objects or a character string containing the
#'   short name of the template e.g. \code{"IS2"}.
#'
#'   When transforming image data (\code{imagedata=TRUE}), the \code{target}
#'   argument should normally be specified. This defines the absolute/voxel
#'   dimensions of the target space. This can be calculated from a
#'   \code{templatebrain} object, so by default it will be set to the value of
#'   the \code{reference} argument. Alternatively an image file on disk can be
#'   specified; this is essential if the \code{reference} argument does not
#'   specify a \code{\link{templatebrain}} object but instead just names a
#'   template space (i.e. is a string).
#'
#'   The significance of the \code{imagedata} and \code{checkboth} arguments is
#'   that CMTK registrations are not directly invertible although they can be
#'   numerically inverted in most cases (unless there are regions where folding
#'   occurred). For image data, numerical inversion is \emph{much} slower.
#'
#'   You can control whether you want to allow inverse registrations manually by
#'   setting \code{checkboth} explicitly. Otherwise when \code{checkboth=NULL}
#'   the following default behaviour occurs: \itemize{
#'
#'   \item when \code{via=NULL} \code{checkboth=T} but a warning will be given
#'   if an inversion must be used.
#'
#'   \item when \code{via} is specified then \code{checkboth=T} but a warning
#'   will be given if an inversion must be used.
#'
#'   }
#'
#' @param x the 3D object to be transformed
#' @param sample Source template brain (e.g. IS2) that data is currently in.
#' @param reference Target template brain (e.g. IS2) that data should be
#'   transformed into.
#' @param via optional intermediate brain to use when there is no direct
#'   bridging registration.
#' @param imagedata Whether \code{x} should be treated as image data (presently
#'   only supported as a file on disk) or 3D object vertices - see details.
#' @param checkboth When \code{TRUE} will look for registrations in both
#'   directions. See details.
#' @param target When transforming image data, this specifies the target space
#'   (defaults to \code{reference} when \code{imagedata=TRUE}). See Details.
#' @param ... extra arguments to pass to \code{\link[nat]{xform}} and then on to
#'   \code{\link[nat]{xformpoints}} or \code{\link[nat]{xformimage}} which will
#'   eventually hand off to \code{\link{cmtk.reformatx}} when using CMTK.
#'
#' @export
#' @seealso \code{\link{mirror_brain}}, \code{\link{regtemplate}},
#'   \code{\link{xform}}. \code{\link{xformpoints}}, \code{\link{xformimage}},
#'   \code{\link{cmtk.reformatx}} (for transforming image data with CMTK).
#' @examples
#' ## depends on nat.flybrains package and system CMTK installation
#' \dontrun{
#' ## reformat neurons
#' ##
#' library(nat.flybrains)
#' # Plot Kenyon cells in their original FCWB template brain
#' nopen3d()
#' plot3d(kcs20)
#' plot3d(FCWB)
#' # Convert to JFCR2 template brain
#' kcs20.jfrc2=xform_brain(kcs20, sample = FCWB, reference=JFRC2, .progress='text')
#' # now plot in the new JFRC2 space
#' nopen3d()
#' plot3d(kcs20.jfrc2)
#' plot3d(JFRC2)
#' # compare with the untransformed neurons
#' plot3d(kcs20)
#' # plot with neuropil sub regions for the left mushroom body
#' clear3d()
#' plot3d(kcs20.jfrc2)
#' # nb "MB.*_L" is a regular expression
#' plot3d(JFRC2NP.surf, "MB.*_L", alpha=0.3)
#' # compare with originals - briging registration is no perfect in peduncle
#' nopen3d()
#' plot3d(kcs20)
#' plot3d(FCWBNP.surf, "MB.*_L", alpha=0.3)
#'
#'
#' ## reformat image examples
#' # see ?cmtk.reformatx for details of any additional arguments
#' # note that for image data a target space defining the dimensions of the
#' # output image must be specified - this happens by default using the
#' # reference templatebrain object
#' xform_brain('in.nrrd', sample=FCWB, ref=JFRC2, output='out.nrrd')
#' # or you can specify an image file explicitly as target
#' xform_brain('in.nrrd', sample=FCWB, ref=JFRC2, output='out.nrrd',
#'             target='JFRC2.nrrd')
#'
#' # use partial volume interpolation for label field
#' xform_brain('labels.nrrd', sample=FCWB, ref=JFRC2, output='out.nrrd',
#'             interpolation='pv')
#'
#' # use binary mask to restrict (and speed up) reformatting
#' xform_brain('in.nrrd', sample=FCWB, ref=JFRC2, output='out.nrrd', mask='neuropil.nrrd')
#' }
xform_brain <- function(x, sample=regtemplate(x), reference, via=NULL,
                        imagedata=is.character(x), checkboth=NULL, target=NULL,
                        ...) {
  if(is.null(sample))
    stop("Invalid sample argument!\n",
         "Either specify manually or use regtemplate(x) <- to set space for x.")
  if(is.null(via)) {
    # use bridging_graph, with checkboth = TRUE
    if(is.null(checkboth)) checkboth=TRUE
    # use imagedata to choose reciprocal weight
    regs<-shortest_bridging_seq(sample = sample, reference = reference,
                          checkboth = checkboth, imagedata = imagedata)
  } else {
    if(is.null(checkboth))
      checkboth=!imagedata
    regs <- bridging_sequence(reference=reference, sample=sample, via=via,
                              checkboth = checkboth, mustWork = T)
  }
  xt <- if(is.null(regs)) {
    x
  } else if(imagedata) {
    if(is.null(target)) {
      if(!is.templatebrain(reference))
        stop("Please specify a <target> image to define the space for reformatting!")
      target=reference
    }
    nat::xform(x, reg=regs, target=target, ...)
  } else {
    nat::xform(x, reg=regs, ...)
  }

  # always set space if this is a complex object otherwise only on input
  if(is.object(x) || !is.null(regtemplate(x))) regtemplate(xt)=reference
  xt
}

#' Mirror 3D object around a given axis, optionally using a warping registration
#'
#' @param x the 3D object to be mirrored.
#' @param brain source template brain (e.g. IS2) that data is in.
#' @param mirrorAxis the axis to mirror (default \code{"X"}).
#' @param transform whether to use warp (default) or affine component of
#'   registration, or simply flip about midplane of axis.
#' @param ... extra arguments to pass to \code{\link[nat]{mirror}}.
#' @seealso \code{\link{xform_brain}}, \code{\link{regtemplate}}
#' @export
#' @examples
#' data(FCWB.demo)
#' # Simple mirror along the x i.e. medio-lateral axis
#' kcs20.flip=mirror_brain(kcs20, FCWB.demo, transform='flip')
#'
#' ## Full non-rigid mirroring to account for differences in shape/centering of
#' ## template brain.
#' ## Depends on nat.flybrains package and system CMTK installation
#' \dontrun{
#' library(nat.flybrains)
#' kcs20.right=mirror_brain(kcs20, FCWB, .progress='text')
#' plot3d(kcs20, col='red')
#' plot3d(kcs20.right, col='green')
#' # include surface plot of brain
#' plot3d(FCWB)
#'
#' # Compare simple flip with full mirror
#' # This template brain is highly symmetric so these are almost identical
#' clear3d()
#' plot3d(kcs20.flip, col='blue')
#' plot3d(kcs20.right, col='green')
#'
#' # Convert to JFRC2 and do the same
#' kcs20.jfrc2=xform_brain(kcs20, sample = FCWB, reference=JFRC2, .progress='text')
#' kcs20.jfrc2.right=mirror_brain(kcs20.jfrc2, JFRC2, .progress='text')
#' kcs20.jfrc2.flip=mirror_brain(kcs20.jfrc2, JFRC2, transform='flip')
#' clear3d()
#' # This time there is a bigger difference between the two transformations
#' plot3d(kcs20.jfrc2.flip, col='blue')
#' plot3d(kcs20.jfrc2.right, col='green')
#' # plot mushroom body neuropils as well
#' plot3d(JFRC2NP.surf, "MB.*_R", alpha=0.3, col='grey')
#'
#' # Compare Euclidean distance between corresponding points in all neurons
#' diffs=xyzmatrix(kcs20.jfrc2.flip)-xyzmatrix(kcs20.jfrc2.right)
#' hist(sqrt(rowSums(diffs^2)), xlab='Distance /microns')
#' }
mirror_brain <- function(x, brain=regtemplate(x), mirrorAxis=c("X","Y","Z"),
                         transform = c("warp", "affine", "flip"), ...) {
  transform=match.arg(transform)
  if(is.null(brain))
    stop("Invalid brain argument!\n",
         "Either specify manually or use regtemplate(x) <- to set space for x.")
  warpfile <- if(transform=="flip") NULL else mirror_reg(brain)
  mirrorAxis <- match.arg(mirrorAxis)
  axisCol <- which(mirrorAxis == c("X", "Y", "Z"))
  mirrorAxisSize <- sum(brain$BoundingBox[1:2, axisCol])
  xm=nat::mirror(x, mirrorAxisSize=mirrorAxisSize, mirrorAxis=mirrorAxis, warpfile=warpfile, transform=transform, ...)
  # always set space if this is a complex object otherwise only on input
  regtemplate(xm) <- if(is.object(x)) brain else regtemplate(x)
  xm
}

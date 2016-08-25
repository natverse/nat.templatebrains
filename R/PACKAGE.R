#' NeuroAnatomy Toolbox add-on package for handling template brains
#'
#' This package provides a class \code{templatebrain} that stores key
#' information about reference brains along with helper functions to simplify
#' transformation of data between template brains (a.k.a bridging) and mirroring
#' of data within a template brain.
#'
#' @section Helper functions: Easy-to-use functions for transforming data from
#'   one template brain to another, displaying slices alongside 3D data, etc.
#'   are provided.
#'
#'   For transforming between spaces, see especially \code{\link{xform_brain}},
#'   \code{\link{mirror_brain}}. The \code{\link{regtemplate}} functions allow
#'   an R object containing neuroanatomical data to be tagged as in a particular
#'   template brain space.
#'
#'   \code{\link{plot3d.templatebrain}} provides a simple way to plot a surface
#'   object representing a given template brain.
#'
#' @section Registrations: Functions such as \code{\link{xform_brain}} provided
#'   by \code{nat.templatebrains} are not very useful unless you tell the
#'   package about bridging/mirroring registrations that you have avilable.
#'   There are 4 supported ways to add registrations to the search list that
#'   will be considered: \itemize{
#'
#'   \item Install a package (e.g. nat.flybrains) that bundles registrations.
#'
#'   \item \code{\link{download_reg_repo}} to download pre-packaged registration
#'   folders from github. This will automatically add the new folders to the
#'   registration search list both in the current session and on package
#'   startup.
#'
#'   \item \code{\link{add_reg_folders}} to add additional local directories to
#'   the registration search list. You will need to do this each time your start
#'   an R session unless you add it to your \code{\link[base]{Rprofile}}.
#'
#'   \item \code{\link{add_reglist}} to add an in-memory
#'   \code{\link[nat]{reglist}} containing an arbitrary sequence of
#'   registrations. By default this will not persist across R sessions but this
#'   behaviour can be changed.
#'
#'   }
#'
#'   Note that the package implements a cache to avoid rescanning the
#'   directories in the registration search list all the time. The functions
#'   mentioned above will automatically ensure that the cache is reset whenever
#'   a new set of registrations are added to the search list.
#'
#' @section Package options:
#'
#'   \itemize{
#'
#'   \item options('nat.templatebrains.regdirs') specifies a character vector of
#'   directories containing registrations i.e. a registration search list.
#'
#'   }
#'
#'   It is strongly recommended that you use the functions mentioned above
#'   rather than manipulating the registration search list directly.
#'
#'   Note that registration directories will be searched in the order that they
#'   are listed. It is therefore strongly recommended that individual
#'   registrations have globally unique names.
#'
#' @name nat.templatebrains-package
#' @aliases nat.templatebrains
#' @seealso \code{\link[nat]{nat}}
#' @docType package
#' @keywords package registration template
#' @examples
#' \dontrun{
#' # Plot all known registrations
#' plot(bridging_graph(), vertex.size=25)
#'
#' ## manually add a new directory containing registrations to the search list
#' # Don't do this unless essential!
#' options(nat.templatebrains.regdirs=union(
#'   getOption('nat.templatebrains.regdirs'), "/my/new/path"))
#' # NB after mucking around with paths manually you must also manually update
#' # the cache so that new registrations are actually used.
#' nat.templatebrains:::reset_cache()
#'
#' ## same, but override any built-in registration by putting the new path
#' # at the first position in the search list
#' options(nat.templatebrains.regdirs=union("/my/new/path"),
#'   getOption('nat.templatebrains.regdirs'))
#' }
NULL

#' Sample template brain: FlyCircuit Whole Brain
#'
#' This is a sample template brain for testing purposes which is equivalent to
#' the FCWB template brain defined by the \code{nat.flybrains}, which should be
#' considered the canonical version.
#' @name FCWB.demo
#' @docType data
NULL

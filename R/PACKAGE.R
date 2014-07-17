#' NeuroAnatomy Toolbox add-on package for handling template brains
#'
#' This package provides a class \code{templatebrain} that stores key
#' information about referennce brains along with helpfer functions to simplify
#' transformation of data between template brains (a.k.a bridging) and mirroring
#' of data within a template brain. Presently there
#'
#' @section Helper functions: Easy-to-use functions for transforming data from
#'   one template brain to another, displaying slices alongside 3D data, etc.
#'   are provided. See especially \code{\link{xform_brain}},
#'   \code{\link{mirror_brain}} and \code{\link{plot3d.templatebrain}}.
#'
#' @section Package options:
#'
#'   \itemize{
#'
#'   \item options('nat.flybrain.extrabridge') specifies a character vector of
#'   additional directories containing bridging registrations.
#'
#'   \item options('nat.flybrain.extramirror') specifies a character vector of
#'   additional directories containing mirroring registrations.
#'
#'   }
#'
#'   Note that registrations in these extra directories will override those of
#'   the same name provided with the package.
#' @name nat.templatebrains-package
#' @aliases nat.templatebrains
#' @seealso \code{\link[nat]{nat}}
#' @docType package
#' @keywords package registration template
NULL

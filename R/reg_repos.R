#' Download and register git repository containing registrations
#'
#' Note that these extra registrations will be downloaded to a standard location
#' on your hard drive that will be used for one session to the next. See
#' examples and \code{\link{local_reg_dir_for_url}}.
#'
#' @param url Location of one or more remote git repositories. Can accept
#'   partial github specifications of the form "<user>/<repo>".
#' @param localdir Full path to local checkout location of git repository. When
#'   \code{localdir=NULL}, the default, a sensible location is chosen using the
#'   rappdirs function.
#' @param ... additional arguments passed to \code{git2r::clone} e.g.
#'   credentials for private repo.
#' @seealso \code{\link{add_reg_folders}}, \code{\link{local_reg_dir_for_url}},
#'   \code{git2r::\link[git2r]{clone}}
#' @return This function is principally called for its side effect. A path to
#'   the location on disk containing added registrations is returned invisibly.
#' @examples
#' ## find the root location of all registration directories
#' local_reg_dir_for_url()
#' \dontrun{
#' ## Add the two main jefferislab bridging and mirroring registration
#' # collections for Drosophila brains from github.com.
#' download_reg_repo("jefferislab/BridgingRegistrations")
#' download_reg_repo("jefferislab/MirrorRegistrations")
#'
#' ## update all current registration repositories
#' update_reg_repos()
#' }
#' @seealso \code{\link{update_reg_repos}}
#' @export
download_reg_repo<-function(url, localdir=NULL, ...) {
  if(!requireNamespace('git2r'))
    stop("Please:\n  install.packages('git2r')\nin order to use this function!")
  url=make_reg_url(url)
  if(length(url)>1)
    return(sapply(url, download_reg_repo, localdir=localdir, ...=..., simplify = FALSE))
  if(is.null(localdir))
    localdir = local_reg_dir_for_url(url)

  if(file.exists(localdir)) {
    update_reg_repos(localdir)
  } else {
    # ensure that the root directory exists
    dir.create(dirname(localdir), showWarnings = FALSE, recursive = TRUE)
    git2r::clone(url, localdir, ...)
    add_reg_folders(localdir)
  }
}


#' Set or list local folders containing registrations for nat.templatebrains
#'
#' @description \code{add_reg_folders} sets
#'   options('nat.templatebrains.regdirs') appropriately so that registrations
#'   can be found by e.g. \code{xform_brain}.
#' @details When \code{dir} is unset then it will default to the value of
#'   \code{extra_reg_folders()} i.e. any folders / cloned repositories in the
#'   standard location
#' @section File layout: You must pass a folder containing one or more
#'   registrations, not the registration folder itself. So if you have this
#'   situation on disk \itemize{
#'
#'   \item myregistrations/
#'
#'   \item myregistrations/reg1.list
#'
#'   \item myregistrations/reg2.list
#'
#'   }
#'
#'   you should write \code{add_reg_folders("/path/to/myregistrations")}
#'
#' @param dir Path to one or more folders containing registrations. Default
#'   value will scan for registration folders in a standard location. (Please
#'   see \bold{Details} and \bold{File layout} sections)
#' @param first Whether the new folder should be added to the start (default) or
#'   end of the search list.
#' @export
#' @return This function is principally called for its side effect. A path to
#'   the location on disk containing added registrations is returned invisibly.
#' @examples
#'
#' \dontrun{
#'   add_reg_folders("myextraregistrations")
#' }
#' # adding a non-existent folder will generate an error
#' tools::assertError(add_reg_folders(tempfile()))
add_reg_folders<-function(dir=extra_reg_folders(), first=TRUE) {
  if(!length(dir))
    return(invisible(NULL))
  if(length(dir)>1)
    return(sapply(rev(dir), add_reg_folders, first=first, simplify = FALSE))

  dir=normalizePath(dir, mustWork = TRUE)

  if(isTRUE(try(nat::is.cmtkreg(dir, filecheck = 'magic'), silent = TRUE))) {
    stop("You passed me CMTK registration folder: ", dir,
         "but I really want its parent folder, so do:\n",
         "add_reg_folders(",dirname(dir),")")
  }

  if(first) {
    options(nat.templatebrains.regdirs=union(dir,
      getOption('nat.templatebrains.regdirs')))

  } else {
    options(nat.templatebrains.regdirs=union(
      getOption('nat.templatebrains.regdirs'), dir))
  }
  reset_cache()
  invisible(dir)
}


#' Add reglist object describing a bridging/mirroring registration
#'
#' @description By specifying either \code{reference, sample} \emph{or}
#'   \code{mirror} arguments, you can add a bridging or mirroring registration,
#'   respectively, to the list of those in use for \code{\link{xform_brain}} and
#'   \code{\link{mirror_brain}}.
#'
#' @param x A single \code{\link[nat]{reglist}} object (which )
#' @param reference,sample The reference and sample brains (in \code{character}
#'   or \code{templatebrain} form) for a bridging registration.
#' @param mirror The reference brain (in \code{character} or
#'   \code{templatebrain} form) for a mirroring registration.
#' @param temp Whether to store the on disk representation in a session-specific
#'   temporary folder (that will be removed when R closes). Defaults to
#'   \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link{saveRDS}} e.g. to
#'   control compression when the reglist object is saved to disk.
#' @return This function is called for its side effect and has no return value.
#' @export
#' @seealso add_reg_folders
#' @examples
#' \dontrun{
#' library(nat.flybrains)
#' # mirroring registration for a specific template brain object
#' add_reglist(mirroring, mirror=JFRC2013)
#' # equivalent but withhout needing to construct the template
#' add_reglist(mirroring, mirror="JFRC2013")
#'
#' # add a bridging registration between two brains
#' add_reglist(bridging, reference=JFRC2, sample=JFRC2013)
#'
#' }
add_reglist <- function(x, reference=NULL, sample=NULL, mirror=NULL, temp=TRUE,
                        ...) {
  # first make a place to store the registration
  if(temp){
    d <- file.path(tempdir(), 'nat.templatebrains', 'tempreglists')
  } else {
    d <- file.path(local_reg_dir_for_url(), "reglists")
  }
  if(!file.exists(d))
    dir.create(d, recursive = TRUE)
  add_reg_folders(d <- normalizePath(d), first = TRUE)

  # now save it with an appropriate name
  if(!is.null(mirror)) {
    f=paste0(as.character(mirror), "_mirror.rds")
  } else if(is.null(reference) || is.null(sample)) {
    stop("Must supply reference and sample brains to define a bridging registration")
  } else {
    f=paste0(as.character(reference), "_", as.character(sample), ".rds")
  }
  saveRDS(x, file=file.path(d, f), ...)
  reset_cache()
  return(invisible())
}

#' Update local copy of git repository containing registrations
#'
#' When \code{x=NULL} all repositories listed in
#' options(nat.templatebrains.regdirs) are checked to see if they are git
#' repositories and, if yes, they are pulled to update.
#'
#' @param x Path to local checkout of a registration git repository. See details
#'   for meaning of default.
#' @return This function is principally called for its side effect, but does
#'   return a \code{git2r::repository} object containing the path on disk to the
#'   location of the git repository with registration.
#' @export
#' @seealso \code{\link{download_reg_repo}}
update_reg_repos<-function(x=NULL) {
  if(is.null(x)) {
    x=getOption('nat.templatebrains.regdirs')
    if(length(x)==0) return(NULL)
  }
  reset_cache()
  if(length(x)>1) return(sapply(x, update_reg_repos))
  repo=try(git2r::repository(x), silent = TRUE)
  if(!inherits(repo, 'try-error'))
    git_pull_helper(repo)
}

git_pull_helper<-function(repo){
  sig=try(git2r::default_signature(repo), silent = TRUE)
  if(inherits(sig, 'try-error')){
    # just make up a user config since we only ever want to pull this repo
    git2r::config(repo, user.name="Anonymous NAT User",
                  user.email="nat@anon.org")
  }
  git2r::pull(repo)
}

# make registration url from partial specification to github repository
make_reg_url<-function(url) {
  isurl=grepl("http[s]{0,1}://", url)
  url[!isurl]=file.path("https://github.com", url[!isurl])
  url
}

#' Standard local checkout location for extra registration directories
#'
#' @details When called without any argument returns the root directory that
#'   will be inspected for extra registrations. You can put a sub-folder
#'   yourself there manually and then call add_reg_folders, but you are much
#'   better off in general using \code{\link{download_reg_repo}} to install from
#'   a github repository such as this one of ours:.
#'   \href{https://github.com/jefferislab/BridgingRegistrations}{jefferislab/BridgingRegistrations}
#'
#'   Note that this folder will always be the same place on a machine i.e. this
#'   defines a consistent, persistent location on disk to store data across
#'   sessions. You can modify the location of this folder by editing the
#'   \code{R_USER_DATA_DIR} environment variable. While this is not recommended
#'   on a personal machine, it may be necessary on a server. See
#'   \code{rappdirs::\link{user_data_dir}} for details.
#'
#'   When called with a url, a SHA1 hash will be calculated for the URL and
#'   appended to the basepath. This should ensure that locations derived from
#'   different URLs do not clash.
#'
#' @param url Character vector containing a url. When \code{url=NULL} defaults
#'   to giving the base path.
#' @return path(s) containing registrations on disk.
#' @export
#' @seealso \code{\link{download_reg_repo}},
#'   \code{rappdirs::\link{user_data_dir}}
#' @importFrom digest digest
#' @importFrom rappdirs user_data_dir
local_reg_dir_for_url<-function(url=NULL) {
  udd=path.expand(user_data_dir("rpkg-nat.templatebrains", appauthor=NULL))
  basedir=file.path(udd,"regfolders")

  if(length(url)) {
    sha1s=sapply(url, digest, algo="sha1", serialize=FALSE)
    file.path(basedir, sha1s)
  }
  else basedir
}

#' @description \code{extra_reg_folders} lists extra registration folders
#'   present in standard location
#' @param full.names Whether to list full path to registration folders
#' @export
#' @rdname add_reg_folders
extra_reg_folders<-function(full.names=TRUE) {
  list.dirs(local_reg_dir_for_url(), full.names=full.names, recursive = FALSE)
}

#' Download and register git repository containing registrations
#'
#' Note that these extra registrations will be downloaded to a standard location
#' on your hard drive that will be used for one session to the next. See
#' examples.
#'
#' @param url Location of remote git repository. Can accept partial github
#'   specifications of the form "<user>/<repo>".
#' @param localdir Full path to local checkout location of git repository. When
#'   \code{localdir=NULL}, the default, a sensible location is chosen using the
#'   rappdirs function.
#' @param ... additional arguments passed to \code{git2r::clone} e.g.
#'   credentials for private repo.
#' @seealso \code{\link{local_reg_dir_for_url}}, \code{git2r::\link[git2r]{clone}}
#' @examples
#' \dontrun{
#' ## Add the two main jefferislab bridging and mirroring registration
#' # collections for Drosophila brains from github.com.
#' download_reg_repo("jefferislab/BridgingRegistrations")
#' download_reg_repo("jefferislab/MirrorRegistrations")
#'
#' ## update all current registration repositories
#' update_reg_repos()
#'
#' ## find the root location of all registration directories
#' local_reg_dir_for_url()
#' }
#' @seealso \code{\link{update_reg_repos}}
#' @export
download_reg_repo<-function(url, localdir=NULL, ...) {
  if(!requireNamespace('git2r'))
    stop("Please:\n  install.packages('git2r')\nin order to use this function!")
  url=make_reg_url(url)
  if(is.null(localdir))
    localdir = local_reg_dir_for_url(url)

  if(file.exists(localdir)) {
    update_reg_repos(localdir)
  } else {
    git2r::clone(url, localdir, ...)
    add_reg_folder(localdir)
  }
}


#' Tell nat.templatebrains about local folder containing registrations
#'
#' @description This sets options('nat.templatebrains.regdirs') appropriately so
#'   that registrations can be found by e.g. \code{xform_brain}.
#'
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
#'   you should write \code{add_reg_folder("/path/to/registrations")}
#'
#' @param dir Path to one or more folders containing registrations (Please see
#'   \bold{File layout} section for details)
#' @param first Whether the new folder should be added to the start (default) or
#'   end of the search list.
#' @export
#' @examples
#'
#' \dontrun{
#'   add_reg_folder("myextraregistrations")
#' }
#' # adding a non-existent folder will generate an error
#' tools::assertError(add_reg_folder(tempfile()))
add_reg_folder<-function(dir, first=TRUE) {
  if(!length(dir))
    return(invisible(NULL))
  if(length(dir)>1)
    return(sapply(dir, add_reg_folder, first=first))

  dir=normalizePath(dir, mustWork = TRUE)

  if(isTRUE(try(nat::is.cmtkreg(dir, filecheck = 'magic'), silent = TRUE))) {
    stop("You passed me CMTK registration folder: ", dir,
         "but I really want its parent folder, so do:\n",
         "add_reg_folder(",dirname(dir),")")
  }

  if(first) {
    options(nat.templatebrains.regdirs=union(dir,
      getOption('nat.templatebrains.regdirs')))

  } else {
    options(nat.templatebrains.regdirs=union(
      getOption('nat.templatebrains.regdirs'), dir))
  }
  invisible(NULL)
}

#' Update local copy of git repository containing registrations
#'
#' When \code{x=NULL} all repositories listed in
#' options(nat.templatebrains.regdirs) are checked to see if they are git
#' repositories and, if yes, they are pulled to update.
#'
#' @param x Path to local checkout of a registration git repository. See details
#'   for meaning of default.
#' @export
#' @seealso \code{\link{download_reg_repo}}
#' @importFrom rappdirs user_data_dir
update_reg_repos<-function(x=NULL) {
  if(is.null(x)) {
    x=getOption('nat.templatebrains.regdirs')
    if(length(x)==0) return(NULL)
  }
  if(length(x)>1) return(sapply(x, update_reg_repos))
  repo=try(git2r::repository(x), silent = TRUE)
  if(!inherits(repo, 'try-error'))
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
#'   yourself there manually and then call add_reg_folder, but you are much
#'   better off in general using \code{\link{download_reg_repo}} to install from
#'   a github repository such as this one of ours:.
#'   \href{https://github.com/jefferislab/BridgingRegistrations}{jefferislab/BridgingRegistrations}
#'
#'
#'   Note that this folder will always be the same place on a machine i.e. this
#'   defines a consistent, persistent location on disk to store data across
#'   sesssions.
#'
#'   When called with a url, a SHA1 hash will be calculated for the URL and
#'   appended to the basepath. This should ensure that locations derived from
#'   different URLs do not clash.
#'
#' @param url Character vector containing a url. When \code{url=NULL} defaults
#'   to giving the base path.
#' @export
#' @importFrom digest digest
#' @importFrom rappdirs user_data_dir
local_reg_dir_for_url<-function(url=NULL) {
  basedir=file.path(user_data_dir("rpkg-nat.templatebrains"), "regfolders")

  if(length(url)) {
    sha1s=sapply(url, digest, algo="sha1", serialize=FALSE)
    file.path(basedir, sha1s)
  }
  else basedir
}

# list of extra registration repositories checked out in standard location
extra_reg_repos<-function(full.names=TRUE) {
  dir(local_reg_dir_for_url(), full.names=full.names)
}

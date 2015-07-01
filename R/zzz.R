.onLoad <- function(libname, pkgname) {

  # see if we have any extra registrations downloaded by user
  # nb prepend these since we want them to be used in place of
  # those distributed with the package
  add_reg_folder(extra_reg_folders(), first = TRUE)

  invisible()
}

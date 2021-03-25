# update the package when it is loaded
.onAttach <- function(libname, pkgname) {
  # deal with 5 possible cases
  # 1. error in getting the current package version -> update
  # 2. error in getting the github package version -> update
  # 3. current package version < github package version -> update
  # 4. current package version > github package version -> do not update
  # 5. current package version == github package version -> do not update
  # in short, update unless the version numbers match

  # get version of the currently installed package
  current_pkg_version <- tryCatch(
    as.character(utils::packageVersion("exploratory")),
    error = function(e) "unknown")
  # github url
  github_url <- paste0("https://raw.githubusercontent.com/jinkim3/",
                       "exploratory/master/DESCRIPTION")
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(github_url),
    warning = function(w) {"github_desc_read_fail"},
    error = function(e) {"github_desc_read_fail"})
  # get the version number of github version
  if (identical(github_pkg_desc, "github_desc_read_fail")) {
    github_pkg_version <- "unknown"
  } else {
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[
        grep("ersion", github_pkg_desc)])
  }
  # compare versions
  compare_version_result <- tryCatch(
    utils::compareVersion(
      current_pkg_version, github_pkg_version),
    warning = function(w) {999}, # 999 indicates no need for update
    error = function(e) {999})
  # skip update for case 5
  if (current_pkg_version != "unknown" &
      github_pkg_version != "unknown" &
      compare_version_result == 0) {
    startup_message <- paste0(
      "Package attached: exploratory v", current_pkg_version,
      " (same as the most recent version available through GitHub).")
  } else if (
    # skip update for case 4
    current_pkg_version != "unknown" &
    github_pkg_version != "unknown" &
    compare_version_result > 0) {
    startup_message <- paste0(
      "Package attached: exploratory v", current_pkg_version,
      " (probably the most recent version ",
      "available through GitHub).")
  } else {
    # update for all other cases
    exploratory::update_exploratory()
    startup_message <- paste0(
      "\nPackage updated: exploratory v", current_pkg_version,
      "\n\nIf you run into any errors, please try restarting R.")
  }
  packageStartupMessage(startup_message)
}

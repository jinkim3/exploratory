# message to print when loading the package
.onAttach <- function(libname, pkgname) {
  # get current version
  installed_pkg_version <- tryCatch(
    as.character(utils::packageVersion("exploratory")),
    error = function(e) NA)
  # github url
  url <- paste0("https://raw.githubusercontent.com/jinkim3/",
  "exploratory/master/DESCRIPTION")
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(url),
    warning = function(w) {"no_result"},
    error = function(e) {"no_result"})
  update_suggestion <- paste0(
    "This may not be the most recent version of the package available",
    " on github. If you run into errors using the package, please",
    " consider updating the package to the most recent version on",
    " github by running 'update_exploratory()'"
  )
  if (identical(github_pkg_desc, "no_result")) {
    if (is.na(installed_pkg_version)) {
      startup_message <- paste0(
        "Attached package: exploratory (version unknown). ",
        update_suggestion)
    } else {
      startup_message <- paste0(
        "Attached package: exploratory ", installed_pkg_version,
        ". ", update_suggestion)
    }
  } else {
    # get the version number of github version
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[
        grep("ersion", github_pkg_desc)])
    # compare versions
    compare_version_result <- tryCatch(
      utils::compareVersion(
        installed_pkg_version, github_pkg_version),
      warning = function(w) {NA},
      error = function(e) {NA})
    # message to print
    if (is.na(compare_version_result)) {
      if (is.na(installed_pkg_version)) {
        startup_message <- paste0(
          "Attached package: exploratory (version unknown). ",
          update_suggestion)
      } else {
        startup_message <- paste0(
          "Attached package: exploratory ", installed_pkg_version,
          ". ", update_suggestion)
      }
    } else if (compare_version_result == -1) {
      startup_message <- paste0(
        "A more recent version of the package 'exploratory' is ",
        "available on github.\n",
        "Currently installed version: ", installed_pkg_version, "\n",
        "Newer version on github:     ", github_pkg_version, "\n",
        "If you run into errors using the package, please",
        " consider updating the package to the most recent version on",
        " github by running 'update_exploratory()'")
    } else if (compare_version_result == 0) {
      startup_message <- paste0(
        "Attached package: exploratory ", installed_pkg_version,
        " (same ", "version as the most recent version ",
        "available on github.)")
    } else if (compare_version_result == 1) {
      startup_message <- paste0(
        "You are running a newer, but possibly unstable version of ",
        "the package 'exploratory'.\n\n",
        "Currently installed version: ", installed_pkg_version, "\n",
        "Older version on github:     ", github_pkg_version, "\n\n",
        "If you run into errors using the package, please",
        " consider downgrading the package to the older version on",
        " github by running 'update_exploratory()'")
    }
  }
  packageStartupMessage(startup_message)
}

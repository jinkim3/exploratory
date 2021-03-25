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
  pkg_ver_at_check_1 <- tryCatch(
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
      pkg_ver_at_check_1, github_pkg_version),
    warning = function(w) {999}, # 999 indicates no need for update
    error = function(e) {999})
  # skip update for case 5
  if (pkg_ver_at_check_1 != "unknown" &
      github_pkg_version != "unknown" &
      compare_version_result == 0) {
    startup_message <- paste0(
      "Package attached: exploratory v", pkg_ver_at_check_1,
      " (same as the most recent version available through GitHub).",
      "\n\nIf you run into errors, please try restarting R.\n")
  } else if (
    # skip update for case 4
    pkg_ver_at_check_1 != "unknown" &
    github_pkg_version != "unknown" &
    compare_version_result > 0) {
    startup_message <- paste0(
      "Package attached: exploratory v", pkg_ver_at_check_1,
      " (probably the most recent version ",
      "available through GitHub).",
      "\n\nIf you run into errors, please try restarting R.\n")
  } else {
    # update, if possible, for all other cases
    update_result <- tryCatch(
      {
        # unload the package exploratory
        while ("package:exploratory" %in% search()) {
          unloadNamespace("exploratory")
        }
        # force update?
        devtools::install_github(
          "jinkim3/exploratory", force = FALSE, upgrade = FALSE)
        # attach the package
        suppressMessages(library("exploratory"))
        # if updating was successful, indicate so
        "success"
      },
      warning = function(w) {return("warning_while_updating")},
      error = function(e) {return("error_while_updating")})
    # check the package version again
    pkg_ver_at_check_2 <- tryCatch(
      as.character(utils::packageVersion("exploratory")),
      error = function(e) "unknown")
    # if update was successful
    if (update_result == "success") {
      startup_message <- paste0(
        "\nPackage updated. Current version: ", pkg_ver_at_check_2,
        "\n\nIf you run into errors, please try restarting R.")
    } else {
      # if there was a problem with updating
      startup_message <- paste0(
        "\nPackage update may have failed.\n",
        "Current version: ",
        pkg_ver_at_check_2, "\n",
        "Most recent version available on GitHub: ",
        github_pkg_version,
        "\n\nIf you run into errors, please try updating the package ",
        'with the command "update_exploratory()" and restarting R.\n')
    }
  }
  packageStartupMessage(startup_message)
}

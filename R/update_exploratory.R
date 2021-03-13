#' Update the package 'exploratory'
#'
#' Updates the current package 'exploratory' by installing the
#' most recent version of the package from GitHub
#'
#' @param force logical. If \code{force = TRUE}, forces the installation
#' even if the package 'exploratory' from the source has not changed since
#' last install.
#' @param upgrade input for \code{upgrade} argument to be passed on to
#' \code{remotes::install_github}.
#' The default value is \code{FALSE}.
#' An input could be \code{TRUE} or \code{FALSE}, but it could also
#' be one of the following: "default", "ask", "always", or "never".
#' "default" respects the value of the R_REMOTES_UPGRADE
#' environment variable if set, and falls back to "ask" if unset.
#' "ask" prompts the user for which out of date packages to upgrade.
#' For non-interactive sessions "ask" is equivalent to "always".
#' TRUE and FALSE are also accepted and correspond
#' to "always" and "never" respectively.
#'
#' @examples
#' \dontrun{
#' update_exploratory()
#' }
#'
#' @export
update_exploratory <- function(
  force = FALSE,
  upgrade = FALSE) {
  # unload the package exploratory
  while ("package:exploratory" %in% search()) {
    detach("package:exploratory", unload = TRUE, character.only = TRUE)
  }
  # if source is github
  devtools::install_github(
      "jinkim3/exploratory", force = force, upgrade = upgrade)
  # attach the package
  kim::prep("exploratory", silent_if_successful = TRUE)
}

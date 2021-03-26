#' Update the package 'exploratory'
#'
#' Updates the current package 'exploratory' by installing the
#' most recent version of the package from GitHub
#'
#' @param confirm logical. If \code{confirm = TRUE}, the user will
#' need to confirm the update. If \code{confirm = FALSE}, the confirmation
#' step will be skipped. By default, \code{confirm = TRUE}.
#' @return there will be no output from this function. Rather, executing
#' this function will update the current 'exploratory' package by installing
#' the most recent version of the package from GitHub.
#' @examples
#' \dontrun{
#' if (interactive()) {update_exploratory()}
#' }
#'
#' @export
update_exploratory <- function(
  confirm = TRUE) {
  # confirm update
  user_reply <- utils::menu(
    c("Yes.", "No."),
    title = "\nDo you want to try to update the package 'exploratory'?")
  if (user_reply == 1) {
    # unload the package exploratory
    while ("package:exploratory" %in% search()) {
      unloadNamespace("exploratory")
    }
    # if source is github
    remotes::install_github("jinkim3/exploratory")
    # attach the package
    exploratory::prep("exploratory", silent_if_successful = TRUE)
  }
}

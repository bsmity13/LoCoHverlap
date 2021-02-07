# Functions to fit LoCoH home ranges

#' Fit one LoCoH home range
#'
#' Fits a single LoCoH home range to data
#'
#' @param dat `[data.frame]` The location data used for fitting the LoCoH.
#' Should contain the following columns:
#'   * `$x` -- The x-coordinate of the animal's location
#'   * `$y` -- The y-coordinate of the animal's location
#'   * `$t` -- The date and time (as a `POSIXct` object) of the location
#' @param crs `[sp::CRS = NULL]` The (optional) coordinate reference system for
#' the location data.
#' @param type `[character = "a"]` The type of neighbor rule used to create the
#' local hulls. Should be one of `"a"` (default), `"k"`, or `"r"`.
#' @param n `[numeric = NA]` The value to use for the neighbor rule, *i.e.*, the
#' value of either a, k, or r used to fit the local hulls.
#' @param `...` Additional arguments passed to \code{\link{amt::hr_locoh}()}.
#'
#' @details A wrapper for `amt` functions to fit LoCoH home ranges. First
#' creates a `track_xyt` object, then calls \code{\link{amt::hr_locoh}()}.
#'
#' @seealso
#' \code{\link{amt::hr_locoh}()}
#' Getz et al. 2007 (*insert full citation here*)
#'
#' @examples
#' \dontrun{
#' # Example
#' }
#' @export
fit_locoh <- function(dat, crs, type, n, ...) {
  dat %>%
    make_track(x, y, t, crs = crs) %>%
    hr_locoh(n = n, type = type,
             levels = seq(from = 0.01, to = 1, by = 0.01),
             ...)
}

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
#' @param crs `[sf::crs = sf::NA_crs_]` The (optional) coordinate reference
#' system for the location data. May also be passed as an integer representing
#' the EPSG code.
#' @param type `[character = "a"]` The type of neighbor rule used to create the
#' local hulls. Should be one of `"a"` (default), `"k"`, or `"r"`.
#' @param n `[numeric = NA]` The value to use for the neighbor rule, *i.e.*, the
#' value of either a, k, or r used to fit the local hulls.
#' @param ... Additional arguments passed to
#' \code{\link[amt:hr_locoh]{hr_locoh}()}..
#'
#' @details A wrapper for `amt` functions to fit LoCoH home ranges. First
#' creates a `track_xyt` object, then calls
#' \code{\link[amt:hr_locoh]{hr_locoh}()}.
#'
#' @return Returns a LoCoH object of class `hr_locoh` from package `amt`.
#'
#' @seealso
#' \code{\link[amt:hr_locoh]{hr_locoh}()}.
#' Getz et al. 2007 (*insert full citation here*)
#'
#' @examples
#' \dontrun{
#' # Example
#' }
#' @importFrom rlang .data
#' @export
fit_locoh <- function(dat, crs = sf::NA_crs_, type = "a", n, ...) {
  # Check `dat`
  checkmate::assert_data_frame(dat)

  # Check `crs`
  if (!is.na(crs) & !inherits(crs, "crs")){
    stop("Argument `crs` should be NULL or of class \"crs\".",
         "\n  See ?sf::st_crs for details.")
  }

  # Check `type`
  checkmate::assert_choice(type, c("a", "k", "r"))

  # Check `n`
  checkmate::assert_number(n, lower = 0)

  # Fit LoCoH
  locoh <- dat %>%
    amt::make_track(.x = .data$x, .y = .data$y, .t = .data$t, crs = crs) %>%
    amt::hr_locoh(n = n, type = type,
             levels = seq(from = 0.01, to = 1, by = 0.01),
             ...)

  # Return
  return(locoh)
}

#' Calculate a*
#'
#' Calculates suggested starting value for `a` under a-LoCoH
#'
#' @param dat `[data.frame]` The location data used for fitting the LoCoH.
#' Should contain the following columns:
#'   * `$x` -- The x-coordinate of the animal's location
#'   * `$y` -- The y-coordinate of the animal's location
#'   * `$t` -- The date and time (as a `POSIXct` object) of the location
#'
#' @details Calculates a* as the maximum distance between any two points in the
#' dataset.
#'
#' @seealso Getz et al. (2007) *insert full citation here*
#'
#' @return A numeric vector of length 1 with the value of a*
#'
#' @examples
#'
#' data(tracks)
#'
#' winter01 <- tracks[which(tracks$id == "ID01" & tracks$season == "Winter"), ]
#'
#' a_star(winter01)
#'
#' @export
a_star <- function(dat) {
  # Check `dat`
  checkmate::assert_data_frame(dat)

  # Calculate distance matrix
  dm <- stats::dist(dat[, c("x", "y")])

  # Get max
  a <- max(dm)

  # Return
  return(a)
}

#' Create a UD from a LoCoH
#'
#' Converts a fitted LoCoH to a utilization distribution
#'
#' @param locoh `[hr_locoh]` The fitted LoCoH returned by
#' \code{\link{fit_locoh}()}.
#' @param rast `[RasterLayer]` The template raster to estimate the UD on.

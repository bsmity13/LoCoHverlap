# Data documentation

#' Locations of 4 animals across 3 seasons
#'
#' A dataset containing the simulated locations of 4 animals over 315 days.
#'
#' @format A `data.frame` with 5040 rows and 5 fields:
#' \describe{
#'   \item{`id`}{Unique identifier of each animal.}
#'   \item{`t`}{Date and time of location.}
#'   \item{`x`}{Location x-coordinate in UTMs (zone 12, epsg:32612)}
#'   \item{`y`}{Location y-coordinate in UTMs (zone 12, epsg:32612)}
#'   \item{`season`}{Season of corresponding movement behavior (winter,
#'   migration, or summer).}
#' }
#'
#' @source Data simulated by Brian J. Smith using \code{\link{bcrw}()}.
#'
#' @examples
#' \dontrun{
#'
#' # Load ggplot2
#' library(ggplot2)
#'
#' # Load data
#' data(tracks)
#'
#' # View head
#' head(tracks)
#'
#' # Plot map of tracks
#' ggplot(tracks, aes(x = x, y = y, color = id)) +
#'   geom_path() +
#'   coord_sf() +
#'   xlab(NULL) +
#'   ylab(NULL) +
#'   theme_bw()
#'
#' # Map winter only
#' winter <- tracks[which(tracks$season == "Winter"), ]
#' ggplot(winter, aes(x = x, y = y, color = id)) +
#'   geom_path() +
#'   coord_sf() +
#'   xlab(NULL) +
#'   ylab(NULL) +
#'   theme_bw()
#'
#' # Map summer only
#' summer <- tracks[which(tracks$season == "Summer"), ]
#' ggplot(summer, aes(x = x, y = y, color = id)) +
#'   geom_path() +
#'   coord_sf() +
#'   xlab(NULL) +
#'   ylab(NULL) +
#'   theme_bw()
#'
#' }
#'
"tracks"

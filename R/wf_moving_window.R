# Functions for moving-window workflow

# Construct windows ----
# Construct start and end points of windows

#' Construct time windows
#'
#' Constructs time windows for moving window analysis
#'
#' @param start `[POSIXt]` The start date and time for the first window.
#' @param width `[Period]` A \code{\link[lubridate:period]{Period}} object
#' specifying the width of each moving window.
#' @param step `[Period]` A \code{\link[lubridate:period]{Period}} object
#' specifying the space between start times for each moving window.
#' @param end `[POSIXt]` The last date and time to include at the end of a
#' window.
#'
#' @return Returns a `data.frame` with columns:
#' \itemize{
#'   \item `window` An integer indexing the window number
#'   \item `start` The date and time of the window start (class `POSIXct`)
#'   \item `end` The date and time of the window end (class `POSIXct`)
#' }
#'
#' @examples
#'
#' # Load lubridate
#' library(lubridate)
#'
#' # Grab a date/time
#' right_now <- Sys.time()
#'
#' # Construct windows
#' construct_windows(start = right_now,
#'                   width = days(5),
#'                   step = days(1),
#'                   end = right_now + days(100))
#'
#' @export
construct_windows <- function(start, width, step, end) {
  # Check `start`
  checkmate::assert_class(start, "POSIXt")
  # Check `width`
  checkmate::assert_class(width, "Period")
  # Check `step`
  checkmate::assert_class(step, "Period")
  # Check `end`
  checkmate::assert_class(end, "POSIXt")

  # Calculate start times
  ss <- seq.POSIXt(from = start,
                   to = end,
                   by = lubridate::as.difftime(step))

  # Calculate end times
  es <- ss + width

  # Construct data.frame of windows
  df <- data.frame(window = 1:length(ss),
                   start = ss,
                   end = es)

  # Remove any end times after end
  # **Note**: might want to give user an option here
  df <- df[which(df$end <= end), ]

  # Coerce to tibble
  df <- tidyr::as_tibble(df)

  # Set S3 class
  class(df) <- c("mv_wind", class(df))

  # Return
  return(df)
}

# Plot moving windows ----

#' Plot moving windows
#'
#' Visualizes moving windows from `mv_wind` object
#'
#' @param x `[mv_wind]` An `mv_wind` object returned by
#' \code{\link{construct_windows}()}.
#' @param set_par `[logical = TRUE]` Should the function be allowed to set
#' its own graphical parameters?
#' @param ... Arguments passed to `segments()`.
#'
#' @export
plot.mv_wind <- function(x, set_par = TRUE, ...) {
  # Check class
  checkmate::assert_class(x, "mv_wind")

  if (set_par) {

    # Get original graphical parameters
    opar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(opar))

    # Set new parameters
    graphics::par(mar = c(3, 0, 0, 0))
  }

  # Setup plotting variables
  # Dummy for setting plot area
  dummy <- data.frame(x = c(x$start, x$end), y = c(x$window, x$window))

  # Plot
  plot(dummy$x, dummy$y, pch = NA)
  # Add segments
  graphics::segments(x0 = x$start, x1 = x$end,
                     y0 = x$window, y1 = x$window,
                     ...)
  # Return x (invisible)
  return(invisible(x))
}

# Create a nested data.frame with data matching windows ----

#' Nest data into windows
#'
#' Creates a nested `data.frame` from an `mv_wind` object
#'
#' @param dat `[data.frame]` A `data.frame` with location data for one
#' individual. Should have same columns as `dat` for \code{\link{fit_locoh}()}.
#' @param mv_wind `[mv_wind]` An `mv_wind` object returned by
#' \code{\link{construct_windows}()}.
#'
#' @details Uses \code{\link[tidyr:nest]{nest}()} and
#' \code{\link[sqldf:sqldf]{sqldf}()}
#'
#' @examples
#' \dontrun{
#'
#' # Load lubridate
#' library(lubridate)
#'
#' # Load data
#' data(tracks)
#'
#' # Keep 1 individual
#' tk1 <- tracks[which(tracks$id == "ID01"), ]
#'
#' # Construct moving windows
#' wind <- construct_windows(start = ymd_hms("2021-05-15 00:00:00"),
#'                           width = days(5),
#'                           step = days(2),
#'                           end = ymd_hms("2021-06-30 00:00:00"))
#'
#' #Nest
#' nst <- nest_windows(tk1, wind)
#'
#' }
#'
#' @export
nest_windows <- function(dat, mv_wind) {
  # Check `dat`
  checkmate::assert_data_frame(dat)

  # Check `mv_wind`
  checkmate::assert_class(mv_wind, "mv_wind")

  # Join
  df <- sqldf::sqldf(
    "SELECT window, start, end, t, x, y
    FROM mv_wind LEFT JOIN dat
    ON dat.t > mv_wind.start AND dat.t < mv_wind.end",
    drv = "SQLite"
  )

  # Nest
  nst <- tidyr::nest(df, data = c(.data$t, .data$x, .data$y))

  # Return
  return(nst)
}

# Make matrix of comparisons for moving window ----

#' Comparison Matrix for Moving Window
#'
#' Makes a comparison matrix for the moving window analysis
#'
#' @param x An `mv_wind` object returned by
#' \code{\link{construct_windows}()}.
#'
#' @examples
#'
#' # Load lubridate
#' library(lubridate)
#'
#' # Grab a date/time
#' right_now <- Sys.time()
#'
#' # Construct windows
#' wind <- construct_windows(start = right_now,
#'                           width = days(10),
#'                           step = days(10),
#'                           end = right_now + days(100))
#'
#' # Create comparison matrix
#' mat <- comp_mat.mv_wind(wind)
#'
#' @export
comp_mat.mv_wind <- function(x) {

  # Check `x`
  checkmate::assert_class(x, "mv_wind")

  # Create blank matrix
  m <- matrix(0, nrow = nrow(x), ncol = nrow(x))

  # For all rows, just compare to next row
  i <- 1:(nrow(x) - 1)
  j <- 2:nrow(x)
  ij <- cbind(i, j)

  m[ij] <- 1

  # Return
  return(m)
}

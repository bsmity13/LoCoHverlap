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
#' @details Uses \code{\link[sqldf:sqldf]{sqldf}()} to join locations to windows
#' that they fall within. Then uses \code{\link[tidyr:nest]{nest}()} to create
#' a nested `data.frame` holding the location data in a list column.
#'
#'
#' @return Returns a nested `data.frame` (a `tbl`) with columns:
#' \itemize{
#'   \item `window` -- An integer indexing the window number
#'   \item `start` -- The date and time of the window start (class `POSIXct`)
#'   \item `end` -- The date and time of the window end (class `POSIXct`)
#'   \item `data` -- A list column containing `tbl`s with the animal location
#'     data. It has columns:
#'     \itemize{
#'       \item `t` -- The location timestamp (class `POSIXct`)
#'       \item `x` -- The x-coordinate of the location
#'       \item `y` -- The y-coordinate of the location
#'     }
#' }
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

# Wrapper function ----

#' LoCoHverlap Moving Window Analysis
#'
#' Runs a full moving window analysis on a dataset.
#'
#' @param dat `[data.frame | list]` The location data used for fitting the
#' LoCoH. Can be a single `data.frame` or a list of `data.frame`s with the
#' following columns:
#'   * `$x` -- The x-coordinate of the animal's location
#'   * `$y` -- The y-coordinate of the animal's location
#'   * `$t` -- The date and time (as a `POSIXct` object) of the location
#' @param start `[POSIXt]` The start date and time for the first window.
#' @param width `[Period]` A \code{\link[lubridate:period]{Period}} object
#' specifying the width of each moving window.
#' @param step `[Period]` A \code{\link[lubridate:period]{Period}} object
#' specifying the space between start times for each moving window.
#' @param end `[POSIXt]` The last date and time to include at the end of a
#' window.
#' @param crs `[sf::crs = sf::NA_crs_]` The (optional) coordinate reference
#' system for the location data. May also be passed as an integer representing
#' the EPSG code.
#' @param type `[character = "a"]` The type of neighbor rule used to create the
#' local hulls. Should be one of `"a"` (default), `"k"`, or `"r"`.
#' @param n `[numeric = NA]` The value to use for the neighbor rule, *i.e.*, the
#' value of either a, k, or r used to fit the local hulls.
#' @param res `[numeric = 500]` The resolution of the `RasterLayer` to create
#' as the template raster. Units should be in the units of the CRS, *e.g.* if
#' `proj=utm`, units should be in meters and if `proj=longlat`, units should be
#' in degrees. Defaults to 500 (assumes meters).
#' @param verbose `[logical = TRUE]` Should the function report what step it
#' is on? Defaults to `TRUE` and cats updates as it works.
#'
#' @details
#'
#' Current implementation does not accept a custom raster like `fit_locoh()`
#' does.
#'
#' @export
mw_analysis <- function(dat,
                        start,
                        width,
                        step,
                        end,
                        crs = sf::NA_crs_,
                        type = "a",
                        n = a_default(dat),
                        res = 500,
                        verbose = TRUE) {

  # Construct windows
  if (verbose) {
    cat("Constructing windows ... ")
  }
  w <- construct_windows(start = start,
                            width = width,
                            step = step,
                            end = end)

  if (verbose) {
    cat("Done.\n")
  }
  # Create nested df
  if (verbose) {
    cat("Nesting windows ... ")
  }

  nw <- nest_windows(dat, w)

  if (verbose) {
    cat("Done.\n")
  }

  # Fit LoCoHs for each window

  if (verbose) {
    cat("Fitting LoCoHs ... ")
  }
  nw$locoh <- lapply(nw$data,
                     fit_locoh,
                     crs = crs,
                     type = type,
                     n = n)

  if (verbose) {
    cat("Done.\n")
  }

  # Rasterize
  if (verbose) {
    cat("Rasterizing LoCoHs ... ")
  }

  nw$r <- lapply(nw$locoh, rasterize_locoh, res = res)

  if (verbose) {
    cat("Done.\n")
  }

  # Calculate EMD

  if (verbose) {
    cat("Calculating EMD ... ")
  }

  # Create comparison matrix
  mat <- comp_mat.mv_wind(w)

  # Compute EMD
  we <- calc_emd(nw$r, mat)

  if (verbose) {
    cat("Done.\n")
  }

  # Add window details to `we`
  we <- we %>%
    dplyr::left_join(
      dplyr::select(nw, .data$window, .data$start),
      by = c("x" = "window")) %>%
    dplyr::left_join(
      dplyr::select(nw, .data$window, .data$end),
      by = c("y" = "window")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(midpoint = mean(c(.data$start, .data$end))) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$x, .data$y, .data$midpoint, .data$emd, .data$log_emd)

  # Construct list to return
  l <- list(emd = we,
            windows = nw)

  # Set S3 class
  class(l) <- c("mwa", class(l))

  return(l)
}

# Default plot for class 'mwa' ----

#' Plot an `mwa` Object
#'
#' Plots an object of class `mwa`.
#'
#' @param x `[mwa]` An object of class `mwa` returned by
#' \code{\link{mw_analysis}()}.
#' @param set_par `[logical = TRUE]` Should the function be allowed to set
#' its own graphical parameters?
#' @param log `[logical = TRUE]` Should the function plot `log(emd)` (default)
#' or just `emd`?
#' @param ... Arguments passed to `plot.default()`.
#'
#' @export
plot.mwa <- function(x, set_par = TRUE, log = TRUE, ...) {
  # Check class
  checkmate::assert_class(x, "mwa")

  if (set_par) {

    # Get original graphical parameters
    opar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(opar))

    # Set new parameters
    graphics::par(mar = c(3, 4.1, 0, 0))
  }

  # Setup plotting variables

  # Plot
  if (log) {
    plot(x$emd$midpoint, x$emd$log_emd, type = "l",
         xlab = NA, ylab = "log(EMD)")
  } else {
    plot(x$emd$midpoint, x$emd$emd, type = "l",
         xlab = NA, ylab = "EMD")
  }

  # Return x (invisible)
  return(invisible(x))
}

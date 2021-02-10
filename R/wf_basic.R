# Functions for basic workflow

# Fit one LoCoH ----
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

# Calculate a* ----
#' Calculate a*
#'
#' Calculates suggested starting value for `a` under a-LoCoH
#'
#' @param dat `[data.frame | list]` The location data used for fitting the
#' LoCoH. Can be a single `data.frame` or a list of `data.frame`s with the
#' following columns:
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
#' \dontrun{
#' data(tracks)
#'
#' # data.frame
#' winter01 <- tracks[which(tracks$id == "ID01" & tracks$season == "Winter"), ]
#'
#' a_star(winter01)
#'
#' # list
#' winter <- tracks[which(tracks$season == "Winter"), ]
#'
#' winter_list <- split(winter, winter$id)
#'
#' a_star(winter)
#' }
#'
#' @rdname a_star
#' @export
a_star <- function(dat) {
  UseMethod("a_star", dat)
}

#' @rdname a_star
#' @export
a_star.data.frame <- function(dat) {
  # Check `dat`
  checkmate::assert_data_frame(dat)

  # Calculate distance matrix
  dm <- stats::dist(dat[, c("x", "y")])

  # Get max
  a <- max(dm)

  # Return
  return(a)
}

#' @rdname a_star
#' @export
a_star.list <- function(dat) {
  # Check `dat`
  checkmate::assert_list(dat, types = "data.frame")

  # Get a-star from all
  a_all <- lapply(dat, a_star.data.frame)

  # Keep max
  a <- max(unlist(a_all))

  # Return
  return(a)
}

# Rasterize LoCoH ----
#' Create a UD from a LoCoH
#'
#' Converts a fitted LoCoH to a utilization distribution
#'
#' @param locoh `[locoh]` The fitted LoCoH returned by
#' \code{\link{fit_locoh}()}.
#' @param r `[RasterLayer]` The template raster to estimate the UD on.
#' If not specified, a raster will be created using the bounding box of `locoh`
#' and the resolution specified by `res`.
#' @param res `[numeric]` The resolution of the `RasterLayer` to create as the
#' template raster. Ignored if `rast` is provided.
#'
#'
#' @export
rasterize_locoh <- function(locoh, r, res) {

  # Check `locoh`
  checkmate::assert_class(locoh, "locoh")

  # Check whether `r` is provided or should be created
  if (missing(r)) {
    # Check `res`
    if (missing(res)) {
      stop("If `r` is not specified, you must specify `res`.")
    }
    checkmate::assert_numeric(res, lower = 1, max.len = 2, all.missing = FALSE)

    # Create `r`

    # -- Get bounding box
    bb <- amt::hr_isopleths(locoh) %>%
      # Get bounding box from isopleth sf geometries
      sf::st_bbox() %>%
      # Convert bounding box to sf polygon
      sf::st_as_sfc() %>%
      # Convert to SpatialPolygon
      sf::as_Spatial()

    # -- Rasterize bb (should inherit CRS from bb)
    r <- raster::raster(bb, res = res)

  } else {
    # If `r` is provided
    # Check `r`
    checkmate::assert_class(r, "Raster", null.ok = TRUE)
  }

  # Rasterize LoCoH
  rr <- amt::hr_isopleths(locoh) %>%
    raster::rasterize(r, field = "level", fun = "first", background = NA)
  # Convert isopleths to probabilities and renormalize
  v <- raster::getValues(rr)
  v <- 1 - v
  v[is.na(v)] <- 0
  nr <- raster::setValues(rr, v/sum(v, na.rm = TRUE))

  # Return
  return(nr)
}

# Calculate EMD for list of rasters ----
# Note, move::emd() calculates EMD for 1 pair of HRs

#' Calculate EMD
#'
#' Calculates Earth-mover's distance for a list of rasters
#'
#' @param r_list `[list]` List of `r` rasterized home ranges to compare.
#' @param mat `[matrix]` `r` x `r` matrix of indicating whether a row-column
#' pair should have its EMD calculated. Can be logical or integer(ish). EMD
#' will be calculated for pairs where `mat == TRUE` or `mat == 1`. See details.
#' @param check_symmetric `[logical = TRUE]` Controls whether function will check for
#' `mat` for symmetry. See details.
#'
#' @details
#'
#' EMD calculations are symmetric, i.e., `emd(x, y) == emd(y, x)`.
#' These calculations are time-consuming, so you will probably want to avoid
#' providing a symmetric matrix. The argument `check_symmetric` defaults to
#' `TRUE` and causes the function to stop if your matrix is symmetric so you
#' can correct this before proceeding.
#'
#' @examples
#' \dontrun{
#'
#' # Load data
#' data(tracks)
#'
#' # Keep only winter data
#' winter <- tracks[which(tracks$season == "Winter"), ]
#'
#' # Split by ID
#' winter_list <- split(winter, winter$id)
#'
#' # Calculate a for a-LoCoH
#' a <- a_star(winter_list)
#'
#' # Fit LoCoHs
#' locoh_list <- lapply(winter_list, fit_locoh, n = a)
#'
#' # Rasterize
#' r_list <- lapply(locoh_list, rasterize_locoh, res = 500)
#'
#' # Matrix of comparisons
#' comp <- matrix(0,
#' nrow = length(winter_list),
#' ncol = length(winter_list))
#'
#' comp[1, 2] <- comp[2, 3] <- comp[3, 4] <- 1
#'
#' # Calculate EMD
#' EMD <- calc_emd(r_list, mat = comp)
#' }
#'
#' @export
calc_emd <- function(r_list, mat, check_symmetric = TRUE) {

  # Check inputs

  # Check `r_list`
  checkmate::check_list(r_list, types = "RasterLayer")

  # Check `mat`
  checkmate::check_matrix(mat)

  # Check `check_symmetric`
  checkmate::check_logical(check_symmetric)

  # Possibly check for symmetry
  if (check_symmetric) {
    if (isSymmetric(mat)) {
      stop("Matrix `mat` is symmetric. Decrease estimation time by keeping ",
           "only 1 triangle (see ?upper.tri) or ignore this check by setting ",
           "`check_symmetric` = FALSE.")
    }
  }

  # Get pairs of EMD to calculate

  # Coerce to logical
  mode(mat) <- "logical"

  # Get pairs
  pairs <- which(mat, arr.ind = TRUE)

  # Calculate EMD
  emd_vect <- apply(pairs, 1, function(x) {
    move::emd(r_list[[x[1]]], r_list[[x[2]]])
  })

  # Construct data.frame to return
  df <- as.data.frame(cbind(pairs, emd_vect))
  names(df) <- c("x", "y", "emd")
  df$log_emd <- log(df$emd)

  # Return
  return(df)
}

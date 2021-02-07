## code to prepare `tracks` dataset goes here

library(LoCoHverlap)
# Simulate data for 4 individuals
# Dataset needs to be useful for demonstrating:
#   - basic workflow
#     * calculate 1 LoCoH for each of 4 indivs and calculate overlap
#     * need them to exhibit high, medium, low, and no overlap
#   - seasonal workflow
#     * calculate 1 LoCoH for summer and 1 LoCoH for winter for each
#       individual and calculate overlap
#     * need some individuals who migrate and some who do not
#   - moving window workflow
#     * calculate LoCoH every week for each individual and calculate
#       overlap between consecutive polygons
#     * need to capture an individual transitioning from ranging to
#       migrating

# Use UTM Zone 12 (epsg: 32612) with tracks near Logan, UT.


# Have all 4 individuals winter in Cache Valley
#   ID01 winters near Hyrum
#   ID02 winters near Providence
#   ID03 winters near my house
#   ID04 winters on USU campus
# That gives overlap like this:

#      | ID01 | ID02 | ID03 | ID04 |
#------|------|------|------|------|
# ID01 |  --  |  --  |  --  |  --  |
#------|------|------|------|------|
# ID02 |  LO  |  --  |  --  |  --  |
# -----|------|------|------|------|
# ID03 |  NO  |  LO  |  --  |  --  |
# -----|------|------|------|------|
# ID04 |  NO  |  MD  |  HI  |  --  |
# -----|------|------|------|------|

# Have 2 individuals summer in Bear River Range (ID01 and ID03)
# Have 2 individuals summer in Wellsvilles (ID02 and ID04)

# Starting locations
start01 <- c("x" = 427400, "y" = 4611000)
start02 <- c("x" = 429000, "y" = 4618999)
start03 <- c("x" = 433000, "y" = 4620000)
start04 <- c("x" = 432300, "y" = 4621300)

# Winter home range centroid
winter01 <- c("x" = 427222, "y" = 4611706)
winter02 <- c("x" = 429637, "y" = 4618110)
winter03 <- c("x" = 432356, "y" = 4620696)
winter04 <- c("x" = 432367, "y" = 4621395)

# Simulate winter locations
# 24 locations per day, 5 months
n_winter <- 24 * 30 * 5
# Set seed
set.seed(20210206)
# Simulate
wloc01 <- bcrw(start_loc = start01,
               centroid = winter01,
               n_steps = n_winter,
               rho = 0.6,
               beta = 0.1)
wloc02 <- bcrw(start_loc = start02,
               centroid = winter02,
               n_steps = n_winter,
               rho = 0.6,
               beta = 0.1)
wloc03 <- bcrw(start_loc = start03,
               centroid = winter03,
               n_steps = n_winter,
               rho = 0.5,
               beta = 0.2)
wloc04 <- bcrw(start_loc = start04,
               centroid = winter04,
               n_steps = n_winter,
               rho = 0.5,
               beta = 0.2)

# Winter data
wdat <- rbind(ID01 = wloc01,
              ID02 = wloc02,
              ID03 = wloc03,
              ID04 = wloc04)
wdat$id <- substr(rownames(wdat), 1, 4)

# Plot
plot(wdat$x, wdat$y, col = factor(wdat$id))

# Summer home range centroid
summer01 <- c("x" = 446589, "y" = 4625899)
summer02 <- c("x" = 414569, "y" = 4613875)
summer03 <- c("x" = 448796, "y" = 4613795)
summer04 <- c("x" = 415412, "y" = 4611580)

# Simulate migration
# 24 locations per day, 15 days
n_mig <- 24 * 15
# Set seed
set.seed(123456)
# Simulate
mig01 <- bcrw(start_loc = c(wloc01$x[n_winter],
                            wloc01$y[n_winter]),
              centroid = summer01,
              n_steps = n_mig,
              sl_distr = c(shape = 2, scale = 100),
              rho = 0.5,
              beta = 0.6)
mig02 <- bcrw(start_loc = c(wloc02$x[n_winter],
                            wloc02$y[n_winter]),
              centroid = summer02,
              n_steps = n_mig,
              sl_distr = c(shape = 2, scale = 100),
              rho = 0.5,
              beta = 0.6)
mig03 <- bcrw(start_loc = c(wloc03$x[n_winter],
                            wloc03$y[n_winter]),
              centroid = summer03,
              n_steps = n_mig,
              sl_distr = c(shape = 2, scale = 100),
              rho = 0.5,
              beta = 0.6)
mig04 <- bcrw(start_loc = c(wloc04$x[n_winter],
                            wloc04$y[n_winter]),
              centroid = summer04,
              n_steps = n_mig,
              sl_distr = c(shape = 2, scale = 100),
              rho = 0.5,
              beta = 0.6)

# Migration data
mdat <- rbind(ID01 = mig01,
              ID02 = mig02,
              ID03 = mig03,
              ID04 = mig04)
mdat$id <- substr(rownames(mdat), 1, 4)


# Plot
plot(mdat$x, mdat$y, col = factor(mdat$id))

# Simulate summer
# 24 locations per day, 5 months
n_summer <- 24 * 30 * 5
# Set seed
set.seed(987654321)
# Simulate
sloc01 <- bcrw(start_loc = c(mig01$x[n_mig],
                             mig01$y[n_mig]),
               centroid = summer01,
               n_steps = n_summer,
               rho = 0.6,
               beta = 0.1)
sloc02 <- bcrw(start_loc = c(mig02$x[n_mig],
                             mig02$y[n_mig]),
               centroid = summer02,
               n_steps = n_summer,
               rho = 0.6,
               beta = 0.1)
sloc03 <- bcrw(start_loc = c(mig03$x[n_mig],
                             mig03$y[n_mig]),
               centroid = summer03,
               n_steps = n_summer,
               rho = 0.5,
               beta = 0.2)
sloc04 <- bcrw(start_loc = c(mig04$x[n_mig],
                             mig04$y[n_mig]),
               centroid = summer04,
               n_steps = n_summer,
               rho = 0.5,
               beta = 0.2)

# summer data
sdat <- rbind(ID01 = sloc01,
              ID02 = sloc02,
              ID03 = sloc03,
              ID04 = sloc04)
sdat$id <- substr(rownames(sdat), 1, 4)

# Plot
plot(sdat$x, sdat$y, col = factor(sdat$id))

# Assign dates ----
# Winter
wdat$dt <- seq(from = lubridate::ymd_hms("2021-01-01 00:00:00",
                                         tz = "US/Mountain"),
               by = "1 hour",
               length.out = n_winter)
wdat$season <- "Winter"
# Migration
mdat$dt <- seq(from = wdat$dt[n_winter] + lubridate::hours(1),
               by = "1 hour",
               length.out = n_mig)
mdat$season <- "Migration"
# Summer
sdat$dt <- seq(from = mdat$dt[n_mig] + lubridate::hours(1),
               by = "1 hour",
               length.out = n_summer)
sdat$season <- "Summer"

# Combine all ----
comb <- rbind(wdat, mdat, sdat)
row.names(comb) <- NULL
comb$t <- NULL

# Plot
plot(comb$x, comb$y, col = factor(comb$id))

# Thin ----
tracks <- comb
tracks$hour <- as.numeric(format(tracks$dt, "%H"))
tracks <- tracks[which(tracks$hour %in% c(0, 6, 12, 18)), ]
tracks$hour <- NULL

# Re-order
tracks <- tracks[, c("id", "dt", "x", "y", "season")]

# Plot
plot(tracks$x, tracks$y, col = factor(tracks$id))

# usethis ----
usethis::use_data(tracks, overwrite = TRUE)

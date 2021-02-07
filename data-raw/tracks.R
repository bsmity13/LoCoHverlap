## code to prepare `tracks` dataset goes here

usethis::use_data(tracks, overwrite = TRUE)

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

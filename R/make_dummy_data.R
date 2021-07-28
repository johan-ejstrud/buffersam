library(dplyr)
library(geosphere)

set.seed(92)

north <- 72
south <- 59
west <- -56
east <- -48

divide.latitude  <- (north - south) * 0.8 + south
divide.longitude <- (west - east) * 0.6 + east

element <-
  expand.grid(
    latitude = seq(south, north, by=1/4),
    longitude = seq(west, east, by=1/4)
  ) %>%
  mutate(
    stratum = case_when(
      latitude  > divide.latitude & longitude <= divide.longitude ~ "A",
      latitude  > divide.latitude & longitude  > divide.longitude ~ "B",
      latitude <= divide.latitude & longitude <= divide.longitude ~ "C",
      latitude <= divide.latitude & longitude  > divide.longitude ~ "D"
    )
  ) %>%
  mutate(
    elementId = paste0(stratum, latitude, longitude)
  ) %>%
  relocate(
    elementId
  )

#   d1   d2
# +----+---+
# | A  | B | d3
# +----+---+
# | C  | D | d4
# +----+---+

d1 = distHaversine(c(north, west), c(north, divide.longitude))
d2 = distHaversine(c(north, divide.longitude), c(north, east))
d3 = distHaversine(c(north, east), c(divide.latitude, east))
d4 = distHaversine(c(divide.latitude, east), c(south, east))

stratum <-
  data.frame(
    stratum = c("A",   "B",   "C",   "D"),
    area =   c(d1*d3, d2*d3, d1*d4, d2*d4)
    ) %>%
  mutate(
    n_stations = sample(2:20, nrow(.))
  )

save_data <- function(dataset) {
  saveRDS(dataset, file = file.path('data', paste0(deparse(substitute(dataset)), '.Rdata')))
}

save_data(element)
save_data(stratum)

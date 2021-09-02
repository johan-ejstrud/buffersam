library(dplyr)
library(geosphere)

set.seed(92)

north <- 70
south <- 59
west <- -58
east <- -44
border <- 1.5
step <- 1/2

divide.latitude  <- (north - south) * 0.8 + south
divide.longitude <- (west - east) * 0.6 + east

border <-
  expand.grid(
    latitude  = seq(south-border, north+border, by=step),
    longitude = seq(west-border, east+border, by=step)
  ) %>%
  dplyr::filter(
    latitude > north | latitude < south | longitude < west | longitude > east
  ) %>%
  dplyr::mutate(stratum = "border")

element <-
  expand.grid(
    latitude = seq(south, north, by=step),
    longitude = seq(west, east, by=step)
  ) %>%
  dplyr::mutate(
    stratum = dplyr::case_when(
      latitude  > divide.latitude & longitude <= divide.longitude ~ "A",
      latitude  > divide.latitude & longitude  > divide.longitude ~ "B",
      latitude <= divide.latitude & longitude <= divide.longitude ~ "C",
      latitude <= divide.latitude & longitude  > divide.longitude ~ "D"
    )
  ) %>%
  rbind(border) %>%
  dplyr::mutate(
    elementId = paste0(stratum, latitude, longitude)
  ) %>%
  dplyr::relocate(
    elementId
  )

usethis::use_data(element, overwrite = TRUE)

#   d1   d2
# +----+---+
# | A  | B | d3
# +----+---+
# | C  | D | d4
# +----+---+

d1 = geosphere::distHaversine(c(north, west), c(north, divide.longitude))
d2 = geosphere::distHaversine(c(north, divide.longitude), c(north, east))
d3 = geosphere::distHaversine(c(north, east), c(divide.latitude, east))
d4 = geosphere::distHaversine(c(divide.latitude, east), c(south, east))

stratum <-
  data.frame(
    stratum = c("A",   "B",   "C",   "D"),
    area =   c(d1*d3, d2*d3, d1*d4, d2*d4)
  ) %>%
  dplyr::mutate(
    n_stations = sample(2:14, nrow(.))
  )

usethis::use_data(stratum, overwrite = TRUE)

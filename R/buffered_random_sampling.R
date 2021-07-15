library(dplyr)

buffered_random_sampling <- function() {
  element <- readRDS(file.path('data', 'element.Rdata'))
  stratum <-
    readRDS(file.path('data', 'stratum.Rdata')) %>%
    mutate(sampling_density = n_stations/area) %>%
    arrange(sampling_density)

  not_selected <- c(element$element)
  selected <- c("C59.75-56", "C61.5-56")


  #for (i in 1:nrow(stratum)) {
    i=1
    current.stratum <- stratum$stratum[i]
    buffering_distance <- sqrt((2*stratum$area[i]) / (pi*stratum$n_stations[i]))
    temp_not_selected <- c()
    temp_selected <- c()

    repeat {
      candidate.element <- sample(not_selected, 1)

      distance_to_nearest_element =
        sapply(
          c(selected, temp_selected),
          FUN = distance_between_elements,
          element2 = candidate.element
        ) %>%
        min()

      if (distance_to_nearest_element > buffering_distance) {
        temp_selected <- c(temp_selected, candidate.element)
      } else {
        temp_not_selected <- c(temp_not_selected, candidate.element)
      }
      not_selected <- not_selected[not_selected != candidate.element]

      n_selected_inside_stratum <-
        filter(element, element %in% temp_selected & stratum == current.stratum) %>%
        nrow()

      if (length(not_selected == 0)) {
        buffering_distance <- buffering_distance * .9
        not_selected <- c(not_selected, temp_selected, temp_not_selected)
        temp_not_selected <- c()
        temp_selected <- c()
      }

      if (n_selected_inside_stratum = stratum$n_stations[i]) {
        #continue cleanup when all points have been selected
        # add temp_selected from stratum to selected
        # add temp_selected not in stratum to not_selected
        # add temp_not_selected to not_selected
        break
      }
    }
}

buffered_random_sampling()

distance_between_elements <- function(element1, element2) {
  distHaversine(element_coordinates(element1), element_coordinates(element2))
}

element_coordinates <- function(elementname) {
  element %>% filter(element == elementname) %>% select(latitude, longitude)
}

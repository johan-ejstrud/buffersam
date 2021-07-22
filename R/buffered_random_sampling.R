library(dplyr)
library(geosphere)
library(ggplot2)
library(ggforce)
library(hexbin)

distance_between_elements <- function(df, element1, element2) {
  distHaversine(element_coordinates(df, element1), element_coordinates(df, element2))
}

element_coordinates <- function(df, elementname) {
  df %>% filter(element == elementname) %>% select(latitude, longitude)
}

buffered_random_sampling <- function() {
  element <- readRDS(file.path('data', 'element.Rdata'))
  stratum <-
    readRDS(file.path('data', 'stratum.Rdata')) %>%
    mutate(sampling_density = n_stations/area) %>%
    arrange(sampling_density)

  not_selected <- c(element$element)
  selected <- c()

  for (i in 1:nrow(stratum)) {
    i=1
    current_stratum <- stratum$stratum[i]
    stations_required_in_stratum = stratum$n_stations[i]
    buffering_distance <- sqrt((2*stratum$area[i]) / (pi*stations_required_in_stratum))
    temp_not_selected <- c()
    temp_selected <- c()
    n_selected_inside_stratum <- 0

    while (n_selected_inside_stratum < stations_required_in_stratum) {
      print(length(not_selected))
      current_element <- sample(not_selected, 1)
      not_selected <- not_selected[not_selected != current_element]

      distance_to_nearest_element <-
        sapply(
          c(selected, temp_selected),
          FUN = distance_between_elements,
          df = element,
          element2 = current_element
        ) %>%
        unlist() %>%
        min()

      if (distance_to_nearest_element > buffering_distance) {
        temp_selected <- c(temp_selected, current_element)
      } else {
        temp_not_selected <- c(temp_not_selected, current_element)
      }

      n_selected_inside_stratum <-
        filter(element, element %in% temp_selected & stratum == current_stratum) %>%
        nrow()

      if (length(not_selected) == 0) {
        print("Restarting. Decreasing buffer distance.")

        # Restart sampling with reduced buffering distance
        buffering_distance <- buffering_distance * .9
        not_selected <- c(not_selected, temp_selected, temp_not_selected)
        temp_not_selected <- c()
        temp_selected <- c()
      }

      print(paste("Sucessfully allocated for stratum", current_stratum))
      df <-
        element %>%
        data.frame() %>%
        mutate(
          current_element = element == current_element,
          selected = element %in% selected,
          not_selected = element %in% not_selected,
          temp_selected = element %in% temp_selected,
          temp_not_selected = element %in% temp_not_selected
        )

      print(
        ggplot(df, aes(x=longitude, y=latitude)) +
          geom_tile(aes(fill=stratum)) + # Stratum
          scale_fill_brewer(type="qua", palette=4) +
          geom_point(data=subset(df, not_selected), colour="grey") + # All points
          geom_point(data=subset(df, current_element), size=3, colour="black") +
          geom_point(data=subset(df, temp_selected), colour="green") +
          geom_point(data=subset(df, temp_not_selected), colour="red") +
          geom_point(data=subset(df, selected), shape=21, colour="black", fill="green") +
          theme_light()
      )
      browser()
    }

    # Enough stations have been selected. Cleanup before starting next stratum.
    element_in_current_stratum <- element$stratum == current_stratum

    selected <-
      c(selected,
        intersect(temp_selected, element$element[element$stratum == current_stratum])
      )

    not_selected <-
      c(not_selected,
        temp_not_selected,
        intersect(temp_selected, element$element[element$stratum != current_stratum])
      )
  }
}

visualise <- function() {
  df <-
    element %>%
    data.frame() %>%
    mutate(
      current_element = element == current_element,
      selected = element %in% selected,
      not_selected = element %in% not_selected,
      temp_selected = element %in% temp_selected,
      temp_not_selected = element %in% temp_not_selected
    )

  print(
    ggplot(df, aes(x=longitude, y=latitude)) +
      geom_tile(aes(fill=stratum)) + # Stratum
      scale_fill_brewer(type="qua", palette=4) +
      geom_point(data=subset(df, not_selected), colour="grey") + # All points
      geom_point(data=subset(df, current_element), size=3, colour="black") +
      geom_point(data=subset(df, temp_selected), colour="green") +
      geom_point(data=subset(df, temp_not_selected), colour="red") +
      geom_point(data=subset(df, selected), shape=21, colour="black", fill="green") +
      theme_light()
  )

}


buffered_random_sampling()

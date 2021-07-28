library(dplyr)
library(geosphere)
library(ggplot2)

distance_between_elements <- function(df, element1, element2) {
  distHaversine(element_coordinates(df, element1), element_coordinates(df, element2))
}

element_coordinates <- function(df, elementname) {
  df %>% filter(element == elementname) %>% select(latitude, longitude)
}

buffered_random_sampling <- function() {
  element <-
    readRDS(file.path('data', 'element.Rdata')) %>%
    mutate(
      current = FALSE,
      selected = FALSE,
      selectable = TRUE,
      temp_selected = FALSE,
      outside_buffdist_of_selected = TRUE,
      outside_buffdist_of_temp_selected = TRUE
    )

  stratum <-
    readRDS(file.path('data', 'stratum.Rdata')) %>%
    mutate(sampling_density = n_stations/area) %>%
    arrange(sampling_density)

  for (i in 1:nrow(stratum)) {
    i=1
    current_stratum <- stratum$stratum[i]
    stations_required_in_stratum = stratum$n_stations[i]
    stratum_area <- stratum$area[i]

    buffering_distance <- sqrt((2*stratum_area) / (pi*stations_required_in_stratum))

    element_backup <- element # Used to restart if allocation fails for buffer distance

    n_selected_inside_stratum <- 0

    while (n_selected_inside_stratum < stations_required_in_stratum) {
      filter(element, selectable) %>% nrow() %>% print()

      current_element <-
        element %>% filter(selectable) %>% select(elementId) %>% sample(size=1)

      element <- mutate(element, current = ifelse(elementId == current_element, TRUE, FALSE))

      # Remove all points in temp_not_selected that are within the buffering
      # distance of the newly selected point. That way all new selected
      # points are automatically within the allowed distance
      distance_to_current_element <-
        distHaversine(
          p1 = element %>% filter(current) %>% select(longitude, latitude),
          p2 = element %>% select(longitude, latitude)
        )

      if (element[current_element, "stratum"] == current_stratum) {
        element[current_element, "selected"] <- TRUE
        element[distance_to_current_element <= buffering_distance, "outside_buffdist_of_selected"] <- FALSE
      } else {
        element[current_element, "temp_selected"] <- TRUE
        element[distance_to_current_element <= buffering_distance, "outside_buffdist_of_temp_selected"] <- FALSE
      }

      element <-
        element %>%
        mutate(selectable = outside_buffdist_of_selected & outside_buffdist_of_temp_selected)


      visualise(df=element)
      browser()

      n_selected_inside_stratum <-
        element %>%
        filter(selected & stratum == current_stratum) %>%
        nrow()

      n_selectable_elements <- filter(element, selectable) %>% nrow()

      if (n_selectable_elements == 0) {
        print("Restarting. Decreasing buffer distance.")
        browser()

        # Restart sampling with reduced buffering distance
        buffering_distance <- buffering_distance * .9
        element <- element_backup
      }
    }

    # Enough stations have been selected. Cleanup before starting next stratum.
    elements <-
      elements %>%
      mutate(
        temp_selected = FALSE,
        within_buffdist_of_temp_selected = FALSE
      )
  }
}

visualise <- function(df) {
  print(
    ggplot(df, aes(x=longitude, y=latitude)) +
      geom_tile(aes(fill=stratum)) + # Stratum
      scale_fill_brewer(type="qua", palette=4) +
      geom_point(data=subset(df, selectable), colour="grey") + # All points
      # geom_point(data=subset(df, current_element), size=3, colour="black") +
      geom_point(data=subset(df, temp_selected), colour="blue") +
      geom_point(data=subset(df, selected), shape=21, colour="black", fill="green") +
      theme_light()
  )

}


buffered_random_sampling()

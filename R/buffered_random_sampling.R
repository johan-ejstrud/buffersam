library(dplyr)
library(geosphere)
library(ggplot2)

buffered_random_sampling <- function() {
  element <-
    readRDS(file.path('data', 'element.Rdata')) %>%
    mutate(
      current = FALSE,
      selected = FALSE,
      selectable = TRUE,
      temp_selected = FALSE,
    )

  stratum <-
    readRDS(file.path('data', 'stratum.Rdata')) %>%
    mutate(sampling_density = n_stations/area) %>%
    arrange(sampling_density)

  for (i in 1:nrow(stratum)) {
    current_stratum <- stratum$stratum[i]
    stations_required_in_stratum = stratum$n_stations[i]
    stratum_area <- stratum$area[i]

    buffering_distance <- sqrt((2*stratum_area) / (pi*stations_required_in_stratum))
    element$selectable <- TRUE
    element$temp_selected <- FALSE
    element_backup <- element # Used to restart if allocation fails for buffer distance
    selected_elements <- element %>% filter(selected) %>% select(elementId) %>% unlist()
    element <- update_selectable(element, selected_elements, buffering_distance)

    n_selected_inside_stratum <- 0

    repeat {
      random_selectable_element <- sample(which(element$selectable), 1)

      element$current <-
        ifelse(1:nrow(element) == random_selectable_element, TRUE, FALSE)

      # Mark current element as either selected or temp_selected
      if (element[which(element$current), "stratum"] == current_stratum) {
        selection_column <- "selected"
      } else{
        selection_column <- "temp_selected"
      }
      element[which(element$current), selection_column] <- TRUE

      element <- update_selectable(element,
                                   element[which(element$current), "elementId"],
                                   buffering_distance)

      n_selected_inside_stratum <-
        element %>%
        filter(selected & stratum == current_stratum) %>%
        nrow()

      if (n_selected_inside_stratum == stations_required_in_stratum) {
        # visualise(element)
        # browser()

        break
      }

      if ( element %>% filter(selectable & stratum == current_stratum) %>% nrow() == 0) {
        # Restart sampling with reduced buffering distance
        element <- element_backup

        element$selectable <- TRUE
        buffering_distance <- buffering_distance * .9

        selected_elements <- element %>% filter(selected) %>% select(elementId) %>% unlist()
        element <- update_selectable(element, selected_elements, buffering_distance)
      }
    }
  }
}

update_selectable <- function(element, element_Ids, buffering_distance) {
  for (element_Id in element_Ids) {
    distance_to_selected_element <-
      distHaversine(
        p1 = element %>% filter(elementId == element_Id) %>% select(longitude, latitude),
        p2 = element %>% filter(selectable) %>% select(longitude, latitude)
      )

    element[which(element$selectable), "selectable"] <-
      distance_to_selected_element > buffering_distance
  }
  return(element)
}

visualise <- function(df) {
  print(
    ggplot(df, aes(x=longitude, y=latitude)) +
      geom_tile(aes(fill=stratum)) + # Stratum
      scale_fill_brewer(type="qua", palette=4) +
      geom_point(data=subset(df, selectable), colour="grey") + # All points
      geom_point(data=subset(df, temp_selected), colour="blue") +
      geom_point(data=subset(df, selected), shape=21, colour="black", fill="green") +
      theme_light()
  )
}

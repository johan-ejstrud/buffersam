#' Buffered Random Sampling
#'
#' Run the buffered random sampling algorithm.
#'
#' The function requires two data sets and returns a data frame with the result
#' of the buffered random sampling.
#'
#' @param element \code{data.frame} See \code{\link{element}}.
#' @param stratum \code{date.frame} See \code{\link{stratum}}.
#' @param preselect_element \code{data.frame} Vector of elementIds to mark as
#' selected before the algorithm starts running.
#' @param visualise \code{logical} If TRUE a plot of the current stage of the
#' algorithm is shown.
#' @param verbose \code{logical} Print status while the algorithm is running.
#' @param pause \code{logical} Stop the algorithm to inspect progress. Makes
#' most sense when used together with \code{visualise} or \code{verbose}.
#' @param detail \code{numeric}. Control how often \code{visualise},
#' \code{verbose}, and \code{pause} are shown, printed, or executed. Possible
#' values 1, 2, 3, and 4, corresponding to finish, after each stratum, each time
#' the algorithm is reset, or after every element.
#'
#' @return Data frame listing which stratum each element is associated with.
#'
#' @export
#' @importFrom rlang .data
buffered_random_sampling <- function(element, stratum, preselect_element=NULL,
                                     visualise=FALSE, verbose=FALSE,
                                     pause=FALSE, detail=2) {
  element <-
    element %>%
    dplyr::mutate(
      current = FALSE,
      selected = FALSE,
      selectable = TRUE,
      temp_selected = FALSE
    )

  if (typeof(element$latitude) != "double" || typeof(element$longitude) != "double") {
    stop("Latitude and longitude in dataset 'element' need to be of type 'double'.")
  }

  element[which(element$elementId %in% preselect_element), 'selected'] <- TRUE

  stratum <-
    stratum %>%
    dplyr::mutate(sampling_density = .data$n_stations/.data$area) %>%
    dplyr::arrange(.data$sampling_density)

  for (i in 1:nrow(stratum)) {
    current_stratum <- stratum$stratum[i]
    stations_required_in_stratum = stratum$n_stations[i]
    stratum_area <- stratum$area[i]

    n_elements_in_stratum <- element %>% dplyr::filter(stratum == current_stratum) %>% nrow()

    if (stations_required_in_stratum > n_elements_in_stratum) {
      stop(glue::glue("There are fewer elements ({n_elements_in_stratum}) in ",
                      "stratum '{current_stratum}' than the required number of ",
                      "stations ({stations_required_in_stratum}). Increase the ",
                      "number of elements, or reduce the required number of ",
                      "stations.")
           )
    }

    buffering_distance <- sqrt((2*stratum_area) / (pi*stations_required_in_stratum))
    element$selectable <- TRUE
    element$temp_selected <- FALSE
    selected_elements <-
      element %>%
      dplyr::filter(.data$selected) %>%
      dplyr::select(.data$elementId) %>%
      unlist()

    element <- update_selectable(element, selected_elements, buffering_distance)
    element_backup <- element # Used to restart if allocation fails for buffer distance

    if (detail >= 2) {
      if (isTRUE(visualise)) visualise_allocation(element)
      if (isTRUE(verbose)) {
        message(glue::glue("Allocating elements in stratum '{current_stratum}'",
                           "({i} of {nrow(stratum)})"))
        if (detail >= 3) {
          message(glue::glue("  Initial buffering distance is ",
                                            "{round(buffering_distance)} m."))
        }
      }
      if (isTRUE(pause)) wait_for_user_input()
    }

    while (n_selected_in_stratum(element, current_stratum) < stations_required_in_stratum) {
      current_element <- sample(which(element$selectable), 1)

      element$current <- ifelse(1:nrow(element) == current_element, TRUE, FALSE)

      # Mark current element as either selected or temp_selected depending on if
      # it is inside the current stratum
      if (element[which(element$current), "stratum"] == current_stratum) {
        selection_column <- "selected"
      } else{
        selection_column <- "temp_selected"
      }
      element[which(element$current), selection_column] <- TRUE

      element <- update_selectable(element,
                                   element[which(element$current), "elementId"],
                                   buffering_distance)

      if (detail >= 4) {
        if (isTRUE(visualise)) visualise_allocation(element)
        if (isTRUE(verbose)) {
          message(glue::glue("    Selecting element ",
                             "{element[which(element$current), 'elementId']}."))
        }
        if (isTRUE(pause)) wait_for_user_input()
      }

      if (n_selectable_in_stratum(element, current_stratum) == 0) {
        # Restart sampling with reduced buffering distance
        element <- element_backup
        buffering_distance <- buffering_distance * .9

        if (detail >= 3) {
          if (isTRUE(visualise)) visualise_allocation(element)
          if (isTRUE(verbose)) {
            message(glue::glue("  No selectable elements left. Reducing ",
                               "buffering distance to {round(buffering_distance)} m."))
          }
          if (isTRUE(pause)) wait_for_user_input()
        }
      }
    }
    if (detail >= 3) {
      if (isTRUE(visualise)) visualise_allocation(element)
      if (isTRUE(verbose)) message(paste0("  Finished allocation in stratum"))
      if (isTRUE(pause)) wait_for_user_input()
    }
  }

  element$current <- FALSE
  element$temp_selected <- FALSE

  if (detail >= 1) {
    if (isTRUE(verbose)) message("Allocation finished.")
    if (isTRUE(visualise)) visualise_allocation(element)
  }

  allocation <- element %>% dplyr::filter(selected) %>% dplyr::select(elementId, stratum)
  return(allocation)
}

n_selected_in_stratum <- function(element, current_stratum) {
  element %>% dplyr::filter(.data$selected & .data$stratum == current_stratum) %>% nrow()
}

n_selectable_in_stratum <- function(element, current_stratum) {
  element %>% dplyr::filter(.data$selectable & .data$stratum == current_stratum) %>% nrow()
}

update_selectable <- function(element, element_Ids, buffering_distance) {
  for (element_Id in element_Ids) {
    distance_to_selected_element <-
      geosphere::distHaversine(
        p1 =
          element %>%
          dplyr::filter(.data$elementId == element_Id) %>%
          dplyr::select(.data$longitude, .data$latitude),
        p2 =
          element %>%
          dplyr::filter(.data$selectable) %>%
          dplyr::select(.data$longitude, .data$latitude)
      )

    element[which(element$selectable), "selectable"] <-
      distance_to_selected_element > buffering_distance
  }
  return(element)
}

wait_for_user_input <- function() {
  invisible(readline(prompt="Press [enter] to continue"))
}

utils::globalVariables(
  c("longitude", "latitude", "selectable", "temp_selected", "selected",
    "ne_countries", "geom_sf", "coord_sf", "elementId")
)

#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_brewer geom_point theme_light geom_sf coord_sf
visualise_allocation <- function(df) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  print(
    ggplot(data = world) +
      geom_sf() +
      coord_sf(xlim = c(min(df$longitude), max(df$longitude)),
               ylim = c(min(df$latitude), max(df$latitude))) +
      geom_tile(data = df, aes(x=longitude, y=latitude, fill=.data$stratum), alpha=0.4) + # Stratum
      scale_fill_brewer(type="qua", palette=4) +
      geom_point(data=subset(df, selectable), aes(x=longitude, y=latitude), colour="grey") + # All points
      geom_point(data=subset(df, temp_selected), aes(x=longitude, y=latitude), colour="blue") +
      geom_point(data=subset(df, selected), aes(x=longitude, y=latitude), shape=21, colour="black", fill="green") +
      theme_light(),
    newpage = FALSE
  )
}

#' Buffered Random Sampling
#'
#' Run the buffered random sampling algorithm.
#'
#' The function requires two data sets and returns a data frame with the result
#' of the buffered random sampling.
#'
#' @param element \code{data.frame} See \code{\link{element}}.
#' @param stratum \code{date.frame} See \code{\link{stratum}}.
#' @param preselect_element \code{data.frame} same format as
#' \code{\link{element}}, but only requires the columns \code{elementId} and
#' \code{stratum}. All elements in this data set are selected before the
#' algorithm starts running.
#' @param visualise \code{logical} If TRUE a plot of the current stage of the
#' algorithm is shown.
#' @param verbose \code{logical} Print status while the algorithm is running.
#' @param pause \code{logical} Stop the algorithm to inspect progress. Makes
#' most sense when used together with \code{visualise} or \code{verbose}.
#' @param detail \code{numeric}. Control how often \code{visualise},
#' \code{verbose}, and \code{pause} are shown, printed, or executed. Possible
#' values 1, 2, 3, and 4, corresponding to finish, after each stratum, each time
#' the algorithm is reset, or after every element.
#' @param tau \code{numeric}. Sets the initial "packing intensity". Default is
#' 0.5. See the paper for more detail.
#'
#' @return Data frame listing which stratum each element is associated with.
#'
#' @examples
#' # See vignette("buffersam") for more examples.
#' allocation <- buffered_random_sampling(element, stratum)
#'
#' @export
#' @importFrom rlang .data
buffered_random_sampling <- function(element, stratum, preselect_element=NULL,
                                     visualise=FALSE, verbose=FALSE,
                                     pause=FALSE, detail=2, tau=0.5) {
  #graphics.off() # Clear plot window

  element <-
    element %>%
    dplyr::mutate(
      current = FALSE,
      selected = FALSE,
      selectable = TRUE,
      temp_selected = FALSE
    )

  if (!is.null(preselect_element)) {
    index_of_preselected_elements <-
      paste0(element$elementId, element$stratum) %in%
      paste0(preselect_element$elementId, preselect_element$stratum)

    element[index_of_preselected_elements, 'selected'] <- TRUE
  }

  element <- assign_duplicated_elements_to_stratum(element)

  stratum <-
    stratum %>%
    dplyr::mutate(sampling_density = .data$n_stations/.data$area) %>%
    dplyr::arrange(.data$sampling_density)

  check_data_sanity(element, stratum)

  for (i in 1:nrow(stratum)) {
    current_stratum <- stratum$stratum[i]
    stations_required_in_stratum = stratum$n_stations[i]
    stratum_area <- stratum$area[i]

    buffering_distance <- sqrt((4*tau*stratum_area) / (pi*stations_required_in_stratum))
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
        buffering_distance <- buffering_distance * .9

        if (detail >= 3) {
          if (isTRUE(visualise)) visualise_allocation(element)
          if (isTRUE(verbose)) {
            message(glue::glue("  No selectable elements left. Reducing ",
                               "buffering distance to {round(buffering_distance)} m."))
          }
          if (isTRUE(pause)) wait_for_user_input()
        }

        element <- element_backup
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

assign_duplicated_elements_to_stratum <- function(element) {
  # Assign elements to *one* stratum by remove duplicated elements at random.
  # If the data contains the column preselect_probability the sampling will be
  # done proportionally to these values. See more in package vignette.

  if (!"preselect_probability" %in% names(element)) {
    element$preselect_probability <- 1
  }

  preselected_elements <- dplyr::filter(element, selected)$elementId

  # Remove duplicates of preselected elements
  element <-
    dplyr::filter(element, ! elementId %in% preselected_elements | selected)

  tmp <- data.frame()

  for (eId in unique(element$elementId)) {
    df <- dplyr::filter(element, eId==elementId)
    row <- df[sample(1:nrow(df), 1, prob=df$preselect_probability), ]

    tmp <- rbind(tmp, row)
  }

  return(tmp)
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

contains_duplicates <- function(c) {
  length(c) != length(unique(c))
}

check_data_sanity <- function(element, stratum) {
  if (typeof(element$latitude) != "double" || typeof(element$longitude) != "double") {
    stop("Latitude and longitude in dataset 'element' need to be of type 'double'.")
  }

  if (contains_duplicates(element$elementId)) {
    stop(glue::glue("There are duplicates in element$elementId. All entires need
                    to be unique"))
  }

  if (contains_duplicates(stratum$stratum)) {
    stop(glue::glue("There are duplicates in stratum$stratum. All entires need
                    to be unique"))
  }

  for (this_stratum in stratum$stratum) {
    n_elements_in_stratum <- element %>% dplyr::filter(stratum == this_stratum) %>% nrow()
    stations_required_in_stratum <- stratum[which(stratum$stratum == this_stratum), ]$n_stations

    if (stations_required_in_stratum > n_elements_in_stratum) {
      stop(glue::glue("There are fewer elements ({n_elements_in_stratum}) in ",
                      "stratum '{this_stratum}' than the required number of ",
                      "stations ({stations_required_in_stratum}). Increase the ",
                      "number of elements, or reduce the required number of ",
                      "stations.")
      )
    }
  }
}

#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_brewer geom_point
#' theme_light geom_sf coord_sf scale_colour_manual
visualise_allocation <- function(df) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  df2 <-
    df %>%
    tidyr::pivot_longer(cols = c(current, selected, selectable, temp_selected),
                        names_to = "type") %>%
    dplyr::filter(value)

  p <-
    ggplot() +
      geom_sf(data = world) +
      coord_sf(xlim = c(min(df$longitude), max(df$longitude)),
               ylim = c(min(df$latitude), max(df$latitude))) +
      geom_tile(data = df, aes(x=longitude, y=latitude, fill=stratum), alpha=0.15) + # Stratum
      geom_point(data=df2, aes(x=longitude, y=latitude, color=type)) +
      scale_colour_manual(name = "Element",
                          values = c("temp_selected" = "blue",
                                     "selectable" = "grey",
                                     "selected" = "green")) +
      theme_light()
  print(p, newpage=FALSE)
}

utils::globalVariables(
  c("longitude", "latitude", "selectable", "temp_selected", "selected",
    "ne_countries", "geom_sf", "coord_sf", "elementId", "current", "stratum",
    "value", "type")
)

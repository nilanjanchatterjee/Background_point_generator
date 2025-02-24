library(move2)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)

BOUNDING_BOX <- "bbox"
MCP <- "mcp"
POPULATION <- "population"
INDIVIDUAL <- "individual"


get_extent <- function(observed_move_data, extent_type, percent = 100) {
  if (extent_type == MCP) {
    centroid <- sf::st_centroid(sf::st_union(observed_move_data))
    dist <- as.numeric(sf::st_distance(observed_move_data, centroid))
    within_percentile_range <- dist <= quantile(dist, percent / 100)
    filtered_data <- st_union(observed_move_data[within_percentile_range, ])
    extent <- st_convex_hull(filtered_data)
  } else {
    extent <- st_as_sfc(st_bbox(observed_move_data))
  }

  return(extent)
}


get_background_points_df <- function(extent, track_id_column,
                                     observed_move_data, points, type = "random") {
  # generate background points
  background_points_df <- st_sample(extent,
    size = nrow(observed_move_data) * points,
    type = type
  ) |>
    st_coordinates() |>
    as.data.frame() |>
    mutate(
      case = 0
    )

  # convert observed data to a data frame to be merged with background points
  observed_data_df <- observed_move_data |>
    st_coordinates() |>
    as.data.frame() |>
    mutate(
      case = 1
    )

  # combine background points with observed points and add additional columns
  all_points <- rbindlist(list(
    observed_data_df,
    background_points_df
  )) |>
    mutate(
      timestamp = seq(
        from = min(observed_move_data$timestamp, na.rm = T),
        to = max(observed_move_data$timestamp, na.rm = T),
        length.out = n()
      ),
      !!track_id_column := rep(mt_track_id(observed_move_data), points + 1)
    )


  return(all_points)
}



rFunction <- function(data, scale, points, extent_type) {
  original_track_id_column <- mt_track_id_column(data)
  track_attribute_data <- mt_track_data(data)

  if (scale == "population") {
    extent <- get_extent(observed_move_data = data, extent_type = extent_type)
    final_data_df <- get_background_points_df(
      extent = extent,
      track_id_column = original_track_id_column,
      observed_move_data = data,
      points = points
    )
  } else {
    filtered_data <- data |>
      filter_track_data(nrow(data) > 5)


    track_list <- list()

    track_ids <- unique(mt_track_id(filtered_data))

    for (i in 1:length(track_ids)) {
      current_track_id <- track_ids[i]
      individual_data <- filtered_data |>
        filter_track_data(.track_id = current_track_id)

      extent <- get_extent(
        observed_move_data = individual_data,
        extent_type = extent_type
      )

      background_point_df <- get_background_points_df(
        extent = extent,
        track_id_column = original_track_id_column,
        observed_move_data = individual_data,
        points = points
      )

      track_list[[i]] <- background_point_df
    }

    final_data_df <- rbindlist(track_list)
  }


  # add track attributes back in
  final_merged_df <- left_join(
    final_data_df,
    track_attribute_data,
    join_by(!!original_track_id_column == !!original_track_id_column)
  ) |>
    dplyr::arrange(!!original_track_id_column, timestamp, case)


  suppressWarnings({
    plot <- ggplot(final_merged_df |> arrange(case), aes(x = X, y = Y)) +
      geom_point(aes(col = as.factor(case), alpha = as.factor(case))) +
      labs(x = "Longitude", y = "Latitude", col = "Point Type") +
      scale_color_discrete(labels = c("Background", "Observed")) +
      scale_alpha_discrete(guide = "none") +
      theme_bw()

    if (scale == INDIVIDUAL) {
      plot <- plot +
        facet_wrap(sym(original_track_id_column))
    }

    ggsave(plot,
      file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "Presence_random_locations.jpeg"),
      width = 10, height = 8, dpi = 200, units = "in"
    )
  })


  # Convert the final data frame back into a move2 stack
  final_move_data <- mt_as_move2(
    final_merged_df,
    coords = c("X", "Y"),
    time_column = "timestamp", crs = 4326,
    track_id_column = original_track_id_column,
    track_attributes = names(track_attribute_data)
  )

  return(final_move_data)
}

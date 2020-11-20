my_correlate <- function(data, vector_name, placement) {
  # my_correlate performs a correlation analysis of the selected variables.
  #
  # Args:
  #   data: A data frame.
  #   vector_name: A character string with the vector name. Either "resultant"
  #   or "vertical".
  #   placement: A character string with the name of the accelerometer
  #   placement (either "ankle", "low back" or "hip").
  #
  # Returns:
  #   A cor_df object.
  msg <- paste("Accelerometer placement:", placement, "-- Vector:", vector_name)
  corr <- data %>%
    dplyr::filter(vector == vector_name & acc_placement == placement) %>%
    dplyr::select(pGRF_N, pACC_g, body_mass, pLR_Ns, pATR_gs) %>%
    corrr::correlate()
  return(list(msg, corr))
}

filter_data <- function(data, vector_name, placement) {
  # filter_data filters data by accelerometer placement and vector.
  #
  # Args:
  #   data: A data frame.
  #   vector_name: A character string with the vector name. Either "resultant"
  #   or "vertical".
  #   placement: A character string with the name of the accelerometer
  #   placement (either "ankle", "low_back" or "hip").
  #
  # Returns:
  #   A data frame.
  dplyr::filter(data, vector == vector_name, acc_placement == placement)
}

prepare_data <- function(data) {
  # prepare_data prepares data to test differences between actual and predicted
  # values among accelerometer placements and activity types.
  #
  # Args:
  #   data: A data frame.
  #
  # Return:
  #   A tibble.
  data %>%
    tidyr::pivot_longer(
      cols = c(actual, predicted),
      names_to = "value_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      value_type = dplyr::recode(
        value_type,
        "predicted" = paste0(acc_placement, "_", vector),
        "actual" = paste0("actual_", vector)
      )
    )
}

bind_data <- function(list) {
  # bind_data combines a list of data frames by row.
  #
  # Args:
  #   list: A list of data frames.
  #
  # Returns:
  #   A data frame.
  purrr::map_dfr(list, rbind)
}

write_data <- function(data, name, path) {
  # write_data writes a data frame into a csv file with the specified filename
  # and path.
  #
  # Args:
  #   data: A data frame.
  #   name, path: A character string.
  #
  # Returns:
  #   Write data as a csv file.
  readr::write_csv(data, paste0(path, name, ".csv"))
}

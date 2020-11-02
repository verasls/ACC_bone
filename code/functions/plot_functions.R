histogram <- function(data, vector_name, var, grid_y, grid_x) {
  # histogram plots a histogram of var with a facet_grid by the specified
  # variables
  #
  # Args:
  #   data: A data frame.
  #   vector_name: A character string with the vector name. Either "resultant"
  #   or "vertical".
  #   var: A character string with the variable name to be plot in the
  #   histogram.
  #   grid_y, grid_x: A character string with the name of the variables to be
  #   used in the facet_grid().
  #
  # Returns:
  #   A ggplot object.
  ggplot2::ggplot(dplyr::filter(data, vector == vector_name)) +
    ggplot2::geom_histogram(aes(x = .data[[var]])) +
    ggplot2::facet_grid(vars(.data[[grid_y]]), vars(.data[[grid_x]])) +
    labs(title = vector_name)
}

box_plot <- function(data, x, y, placement) {
  # box_plot plots a boxplot of y by x, grouping by vector (resultant and 
  # vertical) and labeling outliers.
  #
  # Args:
  #   data: A data frame.
  #   x, y: The name of the variable (unquoted) to be plot in the x and y axis.
  #   placement: A character string with the name of the accelerometer
  #   placement (either "ankle", "low back" or "hip").
  #
  # Returns:
  #   A ggplot object.
  data <- dplyr::mutate(
    dplyr::group_by(data, {{ x }}, vector),
    outlier = ifelse(lvmisc::is_outlier({{ y }}, na.rm = TRUE), subj, NA)
  )
  ggplot2::ggplot(data = data, aes(x = {{ x }}, y = {{ y }}, fill = vector)) +
    ggplot2::geom_boxplot() +
    ggrepel::geom_text_repel(
      aes(label = outlier), na.rm = TRUE, position = position_dodge(1)
    )
}

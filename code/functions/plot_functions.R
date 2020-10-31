histogram <- function(data, vector_name, var, grid_y, grid_x) {
  # histogram plots a histogram of var with a facet_grid by the specified
  # variables
  #
  # Args:
  #   data: A data frame.
  #   vector name: A character string with the vector name. Either "resultant"
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

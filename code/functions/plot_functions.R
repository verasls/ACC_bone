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
    ggplot2::geom_histogram(ggplot2::aes(x = .data[[var]])) +
    ggplot2::facet_grid(
      ggplot2::vars(.data[[grid_y]]), ggplot2::vars(.data[[grid_x]])
    ) +
    ggplot2::labs(title = vector_name)
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
  ggplot2::ggplot(
    data = data, ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = vector)
  ) +
    ggplot2::geom_boxplot() +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = outlier), na.rm = TRUE, position = position_dodge(1)
    ) +
    ggplot2::labs(title = placement)
}

scatterplot <- function(data, x, y, placement) {
  # scatterplot plots a scatterplot of y by x, with different colours by
  # BMI_cat and facets by vector.
  #
  # Args:
  #   data: A data frame.
  #   x, y: The name of the variable (unquoted) to be plot in the x and y axis.
  #   placement: A character string with the name of the accelerometer
  #   placement (either "ankle", "low back" or "hip").
  #
  # Returns:
  #   A ggplot object.
  ggplot2::ggplot(dplyr::filter(data, acc_placement == placement)) +
    ggplot2::geom_point(
      ggplot2::aes(x = {{ x }}, y = {{ y }}, colour = BMI_cat)
    ) +
    ggplot2::facet_grid(~ vector) +
    ggplot2::labs(title = placement)
}

bland_altman <- function(data, title) {
  # bland_altman plots a Bland-Altman plot: a scatterplot of the difference
  # between actual and predicted values by their mean, with a horizontal line
  # at the bias (mean difference) and and one in the upper and lower limits of
  # agreement (bias +/- 1.96 * standard deviation), and runs some statistical
  # tests on the plot data.
  #
  # Args:
  #   data: A data frame with one column for the actual values and another
  #   to the predicted values.
  #
  # Returns:
  #   A list with: a data frame with proportional bias results, a data frame
  #   with t-test results and a ggplot object.
  plot_data <- tibble::tibble(
    mean = (data$actual + data$predicted) / 2,
    diff = data$actual - data$predicted
  )
  
  l <- stats::lm(diff ~ mean, plot_data) %>%
    summary()
  r2 <- broman::myround(l$r.squared, 2)
  l_p_value <- l$coefficients[2, 4]
  l_p_value <- ifelse(
    broman::myround(l_p_value, 3) == "0.000", 
    "<0.001", 
    broman::myround(l_p_value, 3)
  )
  
  t <- stats::t.test(plot_data$diff, mu = 0)
  t_p_value <- t$p.value
  t_p_value <- ifelse(
    broman::myround(t_p_value, 3) == "0.000",
    "<0.001",
    broman::myround(t_p_value, 3)
  )
  
  bias <- lvmisc::bias(data$actual, data$predicted, na.rm = TRUE)
  loa <- lvmisc::loa(data$actual, data$predicted, na.rm = TRUE)
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(ggplot2::aes(x = mean, y = diff)) +
    ggplot2::geom_hline(yintercept = bias) +
    ggplot2::geom_hline(yintercept = loa[[1]], linetype = "dotted") +
    ggplot2::geom_hline(yintercept = loa[[2]], linetype = "dotted") +
    ggplot2::labs(title = title)
  
  list(
    proportional_bias = data.frame(p_value = l_p_value, r2),
    bias_equals_0 = data.frame(p_value = t_p_value),
    plot = p
  )
}

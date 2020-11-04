accuracy <- function(data, model) {
  # accuracy computed some indices of accuracy of a model and builds a
  # Bland-Altman plot
  #
  # Args:
  #   data: A data frame.
  #   model: A lmerMod object.
  #
  # Returns:
  #   A list with two elements: the accuracy indices in a data frame and
  #   the Bland-Altman plot in a ggplot object.
  source(here("code", "functions", "plot_functions.R"))
  outcome <- as.character(formula.tools::lhs(stats::formula(model)))
  actual <- data[[outcome]]
  predicted <- predict(model, data)
  
  R2 <- piecewiseSEM::rsquared(model)[[6]]
  MAE <- lvmisc::mean_error_abs(actual, predicted)
  MAPE <- lvmisc::mean_error_abs_pct(actual, predicted)
  RMSE <- lvmisc::mean_error_sqr_root(actual, predicted)
  
  list(
    indices = data.frame(R2, MAE, MAPE, RMSE),
    plot = bland_altman(actual, predicted)
  )
}

get_coefficients <- function(model) {
  # get_coefficients extract coefficients from a model.
  #
  # Args:
  #   model: A lmerMod object.
  #
  # Returns:
  #   A named numeric vector with the model coefficients.
  coefficients <- stats::coef(summary(model))
  coefficients[, 1]
}

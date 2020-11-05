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

# Define functions to extract data sets
get_training_data <- function(x) rsample::analysis(x)
get_testing_data <- function(x) rsample::assessment(x)

loocv <- function(data, formula) {
  # loocv cross-validates the model using the leave-one-out cross-validation
  #  (LOOCV) approach.
  #
  # Args:
  #   data: A data frame.
  #   formula: The model formula.
  #
  # Returns:
  #   A data frame with the actual and predicted values.
  outcome <- as.character(formula.tools::lhs(formula))
  loocv_split <- rsample::group_vfold_cv(data, group = "subj")
  training_data <- purrr::map(loocv_split$splits, get_training_data)
  testing_data <- purrr::map(loocv_split$splits, get_testing_data)
  
  trained_models <- purrr::map(training_data, ~ lme4::lmer(formula, data = .x))
  predicted <- purrr::map2(
    trained_models, testing_data,
    ~ stats::predict(.x, newdata = .y, allow.new.levels = TRUE)
  ) %>% 
    purrr::as_vector() %>% 
    unname()
  actual <- purrr::map(testing_data, outcome) %>% 
    purrr::as_vector()
  
  if ("jump_type" %in% colnames(data)) {
    activity_type <- data$jump_type
  } else {
    activity_type <- data$speed
  }
  
    tibble::tibble(
    subj = data$subj, acc_placement = data$acc_placement, vector = data$vector,
    activity_type, actual, predicted
  )
}

accuracy <- function(data, formula, actual, predicted) {
  # accuracy computes some accuracy indices.
  #
  # Args:
  #   data: A data frame.
  #   formula: The model formula.
  #   actual, predicted: A numerical vector.
  #
  # Returns:
  #   A data frame with the accuracy indices.
  R2 <- piecewiseSEM::rsquared(lme4::lmer(GRF_formula, data = data))[[6]]
  MAE <- lvmisc::mean_error_abs(actual, predicted)
  MAPE <- lvmisc::mean_error_abs_pct(actual, predicted)
  RMSE <- lvmisc::mean_error_sqr_root(actual, predicted)
  data.frame(R2, MAE, MAPE, RMSE)
}

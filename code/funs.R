# Build regression formula table
#
# Params:
#    model: A list of length 3 with the lmerMod model objects.
#    cv: A list of length 3 with the lvmisc_cv objects.
#    outcome: A character vector with the model outcome. Either "pGRF"
# or "pLR".
#    vector: A character vector with the vector used in the model. Either
# "resultant" or "vertical".
#
# Returns:
#    A tibble with the regression formula table.
build_formula_table <- function(model, cv, outcome, vector) {
  model_accuracy <- purrr::map(
    cv, lvmisc::accuracy, na.rm = TRUE
  )
  R2 <- unname(purrr::map_dbl(model_accuracy, "R2_cond"))
  MAE <- broman::myround(unname(purrr::map_dbl(model_accuracy, "MAE")), 1)
  MAPE <- paste0(
    broman::myround(
      unname(purrr::map_dbl(model_accuracy, "MAPE")) * 100, 1
    ),
    "\\%"
  )
  RMSE <- broman::myround(unname(purrr::map_dbl(model_accuracy, "RMSE")), 1)

  tibble::tibble(
    "Vector" = stringr::str_to_sentence(vector),
    "Accelerometer placement" = c("Ankle", "Lower back", "Hip"),
    "Regression equations" = unname(
      purrr::map_chr(model, get_equation, outcome)
    ),
    "R2" = R2, "MAE" = MAE, "MAPE" = MAPE, "RMSE" = RMSE
  )
}

# Get regression equation formula with coefficients and variables
#
# Params:
#    model: The lmerMod model object.
#    outcome: A character vector with the model outcome. Either "pGRF"
# or "pLR".
#    vector: A character vector with the vector used in the model. Either
# "resultant" or "vertical".
#
# Returns:
#    A character vector with the model equation.
get_equation <- function(model, outcome) {
  model_coefs <- coef(summary(model))[, 1]
  model_coefs[1] <- ifelse(
    model_coefs[1] > 0,
    broman::myround(model_coefs[1], 3),
    paste0("- ", broman::myround(abs(as.numeric(model_coefs[1])), 3))
  )
  model_coefs[-1] <- ifelse(
    model_coefs[-1] > 0,
    paste0(" + ", broman::myround(abs(as.numeric(model_coefs[-1])), 3)),
    paste0(" - ", broman::myround(abs(as.numeric(model_coefs[-1])), 3))
  )

  if (stringr::str_detect(outcome, "GRF")) {
    model_outcome <- "pGRF (N)"
    model_acceleration <- "pACC"
  } else if (stringr::str_detect(outcome, "LR")) {
    model_outcome <- "pLR ($\\mathrm{N\\cdot s^{-1}}$)"
    model_acceleration <- "pAR"
  }

  regression_equation <- glue::glue(
    "{model_outcome} = {model_coefs[1]}{model_coefs[2]}({model_acceleration})\\
    {model_coefs[3]}(body mass)\\
    {model_coefs[4]}({model_acceleration} x body mass)"
  )
  as.character(regression_equation)
}

# Bland-Altman plot regression
#
# Runs a linear regression to test whether the difference of the actual and
# predicted values are explained by their mean.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#    The lm object summary.
bland_altman_regression <- function(cv) {
  data <- get_bland_altman_data(cv)
  summary(stats::lm(diff ~ mean, data))
}

# Bland-Altman plot t-test
#
# Runs a one-sample t-test on the bias value, to test whether it differs
# from 0.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#   The htest object summary.
bland_altman_t_test <- function(cv) {
  data <- get_bland_altman_data(cv)
  t.test(data$diff, mu = 0)
}

# Get Bland-Altman plot data.
#
# Computes the element-wise difference between the  actual and predicted values
# and their mean.
#
# Params:
#    cv: A list of length 3 with the lvmisc_cv objects.
#
# Returns:
#  A tibble with two columns: diff and mean.
get_bland_altman_data <- function(cv) {
  dplyr::transmute(
    cv,
    diff = .actual - .predicted,
    mean = (.actual + .predicted) / 2
  )
}

# Compute some indices of accuracy
#
# Params:
#    actual, predicted: A numeric vector with actual and predicted values.
#
# Returns:
#    A data.frame with 3 columns: MAE, MAPE and RMSE for mean absolute error,
# mean absolute percent error and root mean squared error, respectively.
compute_accuracy <- function(actual, predicted) {
  round(
    data.frame(
      MAE = lvmisc::mean_error_abs(actual, predicted, na.rm = TRUE),
      MAPE = lvmisc::mean_error_abs_pct(actual, predicted, na.rm = TRUE),
      RMSE = lvmisc::mean_error_sqr_root(actual, predicted, na.rm = TRUE)
    ),
    2
  )
}

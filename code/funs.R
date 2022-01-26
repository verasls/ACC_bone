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

# Build table with accuracy indices per jump type
#
# Params:
#    accuracy: A list of length 3 with the lvmisc_accuracy objects.
#    vector: A character vector with the vector used in the model. Either
# "resultant" or "vertical".
#    jump_type: A character vector with the jump type. Either "drop", "box" or
# "continuous".
#
# Returns:
#    A tibble with the table with accuracy indices per jump type.
build_accuracy_table <- function(accuracy, vector, jump_type) {
  data <- tibble::tibble(
    Vector = rep(stringr::str_to_sentence(vector), 3),
    `Accelerometer placement` = c("Ankle", "Lower back", "Hip"),
    R2 = broman::myround(unname(purrr::map_dbl(accuracy, "R2_cond")), 2),
    MAE = broman::myround(unname(purrr::map_dbl(accuracy, "MAE"))),
    MAPE = broman::myround(unname(purrr::map_dbl(accuracy, "MAPE")) * 100),
    RMSE = broman::myround(unname(purrr::map_dbl(accuracy, "RMSE"))),
  )
  names(data)[3:6] <- paste0(names(data)[3:6], "_", jump_type)
  data
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

# Predict mechanical loading using the equations by Veras et al. (2020)
# (doi.org/10.1007/s00198-020-05295-2)
#
# Params:
#    data: A data.frame
#    outcome, acc_placement, vector: A character string
#
# Returns:
#    The input data.frame with the predicted values in the column
# .predicted_Veras2020
predict_Veras2020 <- function(data, outcome, acc_placement, vector) {
  c <- get_coefficients(outcome, acc_placement, vector)
  dplyr::mutate(
    data,
    .predicted_Veras2020 = c$b0 +
      c$b1 * pACC_g +
      c$b2 * (pACC_g^2) +
      c$b3 * body_mass +
      c$b4 * pACC_g * body_mass
  )
}

# Get the mechanical loading prediction equation coefficients
#
# Coefficients from the equations by Veras et al. 2020
# (doi.org/10.1007/s00198-020-05295-2)
#
# Params:
#    outcome, acc_placement, vector: A character string
#
# Returns:
#    A named list with the coefficients as follows:
#      b0: Intercept
#      b1: ACC
#      b2: ACC2
#      b3: body mass
#      b4: ACC:body_mass
get_coefficients <- function(outcome, acc_placement, vector) {
  if (
    outcome == "GRF" & acc_placement == "lower_back" & vector == "resultant"
  ) {
    list(
      b0 = - 698.7031,
      b1 = 1047.5129,
      b2 = - 345.2605,
      b3 = 3.8294,
      b4 = 6.0219
    )
  } else if (
    outcome == "GRF" & acc_placement == "hip" & vector == "resultant"
  ) {
    list(
      b0 = - 300.9909,
      b1 = 522.6850,
      b2 = - 171.5606,
      b3 = 3.9596,
      b4 = 5.3671
    )
  } else if (
    outcome == "GRF" & acc_placement == "lower_back" & vector == "vertical"
  ) {
    list(
      b0 = - 776.8934,
      b1 = 1042.9052,
      b2 = - 336.2115,
      b3 = 6.2132,
      b4 = 5.0805
    )
  } else if (
    outcome == "GRF" & acc_placement == "hip" & vector == "vertical"
  ) {
    list(
      b0 = - 435.7365,
      b1 = 586.6627,
      b2 = - 188.9689,
      b3 = 5.8047,
      b4 = 4.9544
    )
  } else if (
    outcome == "LR" & acc_placement == "lower_back" & vector == "resultant"
  ) {
    list(
      b0 = - 287.0209,
      b1 = 572.7967,
      b2 = - 9.8958,
      b3 = 18.1178,
      b4 = 3.4078
    )
  } else if (
    outcome == "LR" & acc_placement == "hip" & vector == "resultant"
  ) {
    list(
      b0 = - 3510.410,
      b1 = 514.898,
      b2 = - 8.639,
      b3 = 51.937,
      b4 = 2.929
    )
  } else if (
    outcome == "LR" & acc_placement == "lower_back" & vector == "vertical"
  ) {
    list(
      b0 = - 324.0761,
      b1 = 552.8242,
      b2 = - 11.9453,
      b3 = 18.1405,
      b4 = 3.9586
    )
  } else if (
    outcome == "LR" & acc_placement == "hip" & vector == "vertical"
  ) {
    list(
      b0 = - 2687.8662,
      b1 = 407.8434,
      b2 = - 7.6603,
      b3 = 45.8905,
      b4 = 3.8995
    )
  }
}

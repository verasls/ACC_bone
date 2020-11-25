get_equation <- function(outcome, coeffs) {
  acc <- NULL
  if (stringr::str_detect(outcome, "pRGRF")) {
    outcome <- "pRGRF (N)"
    acc <- "pRACC"
  } else if (stringr::str_detect(outcome, "pVGRF")) {
    outcome <- "pVGRF (N)"
    acc <- "pVACC"
  }

  coeffs <- round(coeffs, 4)

  coeffs[1] <- ifelse(
    coeffs[1] > 0,
    coeffs[1],
    paste0(" - ", as.character(abs(coeffs[1])))
  )
  coeffs[-1] <- purrr::map(
    as.numeric(coeffs[-1]),
    ~ ifelse(
      .x > 0,
      paste0(" + ", as.character(abs(.x))),
      paste0(" - ", as.character(abs(.x)))
    )
  )

  glue::glue(
    "{outcome} = {coeffs[1]}{coeffs[2]}({acc})\\
    {coeffs[3]}(body mass)\\
    {coeffs[4]}({acc} x body mass)"
  )
}

accuracy_table <- function(placement, coeffs, accuracy) {
  tibble::tibble(
   "Accelerometer placement" = c(placement, ""),
   "Regression equations" = c(
     get_equation("pRGRF", coeffs$resultant),
     get_equation("pVGRF", coeffs$vertical)
   ),
   "R2" = c(accuracy$resultant$R2, accuracy$vertical$R2),
   "MAE" = c(accuracy$resultant$MAE, accuracy$vertical$MAE),
   "MAPE" = c(accuracy$resultant$MAPE, accuracy$vertical$MAPE),
   "RMSE" = c(accuracy$resultant$RMSE, accuracy$vertical$RMSE)
  ) %>%
  dplyr::mutate(
    R2 = round(R2, 2),
    MAE = round(MAE, 1),
    RMSE = round(RMSE, 1)
  )
}

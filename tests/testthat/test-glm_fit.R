df <- dplyr::tibble(
  PC1 = 0.2,
  PC2 = 0.4,
  PC3 = 0.2,
  PC4 = -0.1,
  PC5 = 0.2,
  case = 1,
  score = 0.3

)
# test_formula <- formula()
#
# test_that("Errors if score is missing", {
#   expect_error(glm_fit(select(df, -score)))
# })

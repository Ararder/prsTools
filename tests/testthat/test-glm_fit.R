df <- dplyr::tibble(
  PC1 = rnorm(100),
  PC2 = rnorm(100),
  PC3 = rnorm(100),
  PC4 = rnorm(100),
  PC5 = rnorm(100),
  outcome_mx = sample(c(1,0), 100, replace=TRUE),
  prs_sda = rnorm(100)

)
df2 <- df %>% dplyr::mutate(outcome_mx = sample(c(1:10), 100, replace = TRUE))
# test_formula <- formula()
#
test_that("Errors if score and pheno is misspeccified", {
  testf <- as.formula("C ~ A + B + 3 + ASD")
  expect_error(glm_fit(select(df, -score)))
})

test_that("Correctly identifies dependant and predictors", {
  testf <- as.formula("C ~ A + B + 3 + ASD")
  expect_equal(get_dependant_and_predictor(testf)[["y"]], "C")
  expect_equal(get_dependant_and_predictor(testf)[["X"]], c("A", "B", "3", "ASD"))
  expect_equal(get_dependant_and_predictor(testf)[["X"]][1], "A")

})

test_that("Throws error if df variable is missing", {
  df <- dplyr::tibble(
    PC1 = rnorm(100),
    PC2 = rnorm(100),
    PC3 = rnorm(100),
    PC4 = rnorm(100),
    PC5 = rnorm(100),
    outcome_mx = sample(c(1,0), 100, replace=TRUE),
    prs_sda = rnorm(100)

  )

  df2 <-  dplyr::mutate(df,outcome_mx = sample(c(1:10), 100, replace = TRUE))
  b <- as.formula("outcome_mx ~ PC1")
  f <- as.formula("outcome_mx ~ prs_sda + PC1")
  expect_error(glm_prs_metrics())
  expect_error(glm_prs_metrics(df2,b,f))
})



test_that("Input parameters must be within bounds", {
  expect_error(e_r2(1.2, 50000))
  expect_error(e_r2(0, 50000))
  expect_error(e_r2(0.5,-2, n=3 ))
  expect_error(e_r2(h=0.5, m=3000, n=-3))
})

test_that("number of cases or controls must be positive integer", {
  expect_error(neff(-3,5))
  expect_error(neff(2,0))
  expect_error(neff("2",3))
  expect_error(neff(3, "2"))
  expect_error(neff(3))
})



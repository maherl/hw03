# load the source code of the functions to be tested
source("./code/functions/regression-functions.R")
regression_object <-lm(Sales~TV, data = Advertising)
reg <- lm(mpg ~ disp + hp, data = mtcars)
reg_sum <- summary(reg)

context("Test for RSS")
test_that("RSS is correct", {
  expect_equal(residual_sum_squares(reg), sum(reg$residuals^2))
})

context("Test for R2")
test_that("R2 is correct", {
  expect_equal(r_squared(regression_object), reg_sum$r.squared)
})

context("Test for TSS")
test_that("TSS is correct", {
  expect_equal(total_sum_squares(reg), sum((mtcars$mpg -mean(mtcars$mpg))^2))
})

context("Test for F-stat")
test_that("F-stat is correct", {
  expect_equal(f_statistic(reg), reg_sum$fstatistic[1])
})

context("Test for RSE")
test_that("RSE is correct", {
  expect_equal(residual_std_error(reg), reg_sum$sigma)
})

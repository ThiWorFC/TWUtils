test_that("formatting() formats numerical values", {
  expect_equal(formatting(x=c(1.6589, NA, 0.15681, 32), n=2), c(" 1.66",""," 0.16","32.00"))
})

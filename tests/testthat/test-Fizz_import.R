test_that("Fizz_import() for Fizz Data", {

  data <- Fizz_import(system.file("extdata", "Fizz data.xlsx", package = "TWUtils"))

  exp <- readr::read_csv(system.file("extdata", "Fizz expected.csv", package = "TWUtils")) |>
    dplyr::mutate(Judge = as.character(Judge))

  expect_equal(data, exp)

})

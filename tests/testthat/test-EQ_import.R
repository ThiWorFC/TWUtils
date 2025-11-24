test_that("EQ_import() for EyeQuestion format file", {

  data <- EQ_import(system.file("extdata", "EQ data.xlsx", package = "TWUtils"), convert2num=TRUE)
  exp <- readr::read_csv(system.file("extdata", "EQ expected.csv", package = "TWUtils")) |>
    dplyr::mutate(dplyr::across(c(Session:Sequence), as.character)) |>
    dplyr::mutate(dplyr::across(c(Judge:Product), as.factor))

  expect_equal(data, exp)

})

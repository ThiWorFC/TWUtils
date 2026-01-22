test_that("tableformat() to transform into flextable", {

  ft <- EQ_import(system.file("extdata", "EQ_data.xlsx", package = "TWUtils"), convert2num=TRUE) |>
    dplyr::group_by(Product) |>
    dplyr::summarize(dplyr::across(tidyselect::where(is.numeric), mean)) |>
    tableformat()

  expect_s3_class(ft, "flextable")

})

test_that("unfill_vec() removes identical following values", {

  input <- tibble::tibble(
    Col1 = c("A", "A", "A", "B", "B"),
    Col2 = c(1, 2, 3, 1, 2)
  )

  output <- tibble::tibble(
    Col1 = c("A", NA, NA, "B", NA),
    Col2 = c(1, 2, 3, 1, 2)
  )

  result <- unfill_vec(input$Col1)

  expect_equal(result, output$Col1)

})


test_that("extract", {

  ffq_test_file <- system.file(package = 'eNutritools', 'extdata/enutri-ffq-export.xlsx')

  expect_true(tibble::is_tibble(extract_food_frequencies(ffq_test_file)))
  expect_true(tibble::is_tibble(extract_food_gday(ffq_test_file)))



})

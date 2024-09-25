library(testthat)

# Function to check results
test_doi_result <- function(result) {

  # Expected standardized matrix
  expected_standardized <- matrix(c(
    4.75, 4.75, 3.75, 4.00, 2.42,
    3.81, 3.67, 4.25, 2.55, 3.50,
    3.50, 3.33, 3.00, 2.25, 3.08,
    4.12, 3.92, 4.00, 2.75, 2.17,
    5.06, 4.33, 3.25, 3.15, 2.92,
    3.94, 3.42, 3.55, 3.00, 2.75
  ), nrow = 6, byrow = TRUE)
  rownames(expected_standardized) <- rownames(result$Standardized_matrix)

  # Test for standardized matrix
  expect_equal(
    round(as.matrix(result$Standardized_matrix), digits = 2),
    expected_standardized
  )

  # Expected normalized matrix
  expected_normalized <- matrix(c(
    0.68, 0.68, 0.23, 0.21, 0.35,
    0.54, 0.52, 0.20, 0.34, 0.50,
    0.50, 0.48, 0.29, 0.38, 0.44,
    0.59, 0.56, 0.21, 0.31, 0.31,
    0.72, 0.62, 0.26, 0.27, 0.42,
    0.56, 0.49, 0.24, 0.29, 0.39
  ), nrow = 6, byrow = TRUE)
  rownames(expected_normalized) <- rownames(result$Normalized_matrix)

  # Test for normalized matrix
  expect_equal(
    round(as.matrix(result$Normalized_matrix), digits = 2),
    expected_normalized
  )

  # Test for expected ranking
  expected_ranking <- c("A5", "A1", "A4", "A2", "A3", "A6")

  expect_equal(
    rownames(result$Ranking),
    expected_ranking
  )

  # Test for expected sorted results
  expected_sorted_ranking <- c(0.5299, 0.5081, 0.4560, 0.4522, 0.4381, 0.4373)

  expect_equal(
    round(result$Ranking$Ranking, digits = 4),
    expected_sorted_ranking
  )
}

# Main test
test_that("Test with data from DOI https://doi.org/10.3390/math8061015", {
  dataset <- matrix(c(
    180, 165, 160, 170, 185, 167,
    10.5, 9.2, 8.8, 9.5, 10, 8.9,
    15.5, 16.5, 14, 16, 14.5, 15.1,
    160, 131, 125, 135, 143, 140,
    3.7, 5, 4.5, 3.4, 4.3, 4.1
  ), nrow = 6, ncol = 5)
  rownames(dataset) <- c("A1", "A2", "A3", "A4", "A5", "A6")
  weights <- c(0.35, 0.25, 0.15, 0.15, 0.10)
  criterion_type <- c('max', 'max', 'min', 'min', 'max')
  ideal <- c(200, 12, 10, 100, 8)
  anti_ideal <- c(120, 6, 20, 200, 2)
  n_i <- 1
  n_k <- 6

  # Error tests
  invalid_criterion_type <- c('max', 'max', 'min', 'invalid', 'max')
  expect_error(
    rafsi_method(dataset, weights, invalid_criterion_type, ideal, anti_ideal, n_i, n_k),
    regexp = "Only 'min' or 'max' are valid for criterion_type."
  )

  invalid_weights <- c(0.35, 0.25, 0.15, 0.15, 0.20) # Sum is not equal to 1
  expect_error(
    rafsi_method(dataset, invalid_weights, criterion_type, ideal, anti_ideal, n_i, n_k),
    regexp = "The sum of the weights must be equal to 1."
  )

  # Function execution and result testing
  result <- rafsi_method(dataset, weights, criterion_type, ideal, anti_ideal, n_i, n_k)
  test_doi_result(result)
})

#' Rank Reversal Problem Using a New Multi-Attribute Model - RAFSI Method for Multi-Criteria Decision Making
#'
#' This function implements the (Ranking of Alternatives Through Functional Mapping of Criterion Sub-Intervals Into a Single Interval) RAFSI method, Rank Reversal Problem Using a New Multi-Attribute Model.
#' More information about the method can be found at https://doi.org/10.3390/math8061015.
#' More information about the implementation at https://github.com/mateusvanzetta/rafsi.
#' used for multi-criteria decision-making problems. It calculates the standardized decision matrix,
#' normalizes the data, applies weights, and returns the final sorted rankings.
#'
#' @param dataset A matrix of criterion values where rows represent alternatives and columns represent criteria.
#' @param weights A numeric vector representing the weights of each criterion. The sum of the weights must be 1.
#' @param criterion_type A character vector indicating the type of each criterion ('max' for maximization, 'min' for minimization).
#' @param ideal A numeric vector representing the ideal values for each criterion.
#' @param anti_ideal A numeric vector representing the anti-ideal values for each criterion.
#' @param n_i A numeric value representing the ratio that shows to what extent the anti-ideal value is worse than the value.
#' @param n_k A numeric value representing the ratio that shows to what extent the ideal value is preferred over the anti-ideal value.
#'
#' @return A list containing:
#' \describe{
#'   \item{Standardized_matrix}{The matrix after applying the RAFSI transformation, which standardizes the data according to the ideal and anti-ideal values.}
#'   \item{Normalized_matrix}{The matrix after normalizing the standardized data, adjusted according to the criteria weights.}
#'   \item{Ranking}{A data frame showing the final ranking of the alternatives. The alternatives are sorted in descending order of preference.}
#' }
#'
#' #'
#' @examples
#' # Define the dataset
#' dataset <- matrix(c(
#'   180, 165, 160, 170, 185, 167,   # Criterion 1
#'   10.5, 9.2, 8.8, 9.5, 10, 8.9,   # Criterion 2
#'   15.5, 16.5, 14, 16, 14.5, 15.1, # Criterion 3
#'   160, 131, 125, 135, 143, 140,   # Criterion 4
#'   3.7, 5, 4.5, 3.4, 4.3, 4.1      # Criterion 5
#' ), nrow = 6, ncol = 5)
#'
#' # Set the names of alternatives
#' rownames(dataset) <- c("A1", "A2", "A3", "A4", "A5", "A6")
#'
#' # Define the weights and criterion types
#' weights <- c(0.35, 0.25, 0.15, 0.15, 0.10)
#' criterion_type <- c('max', 'max', 'min', 'min', 'max')
#'
#' # Specify ideal and anti-ideal values
#' ideal <- c(200, 12, 10, 100, 8)
#' anti_ideal <- c(120, 6, 20, 200, 2)
#'
#' # Set n_i and n_k values
#' n_i <- 1
#' n_k <- 6
#'
#' # Apply the RAFSI method
#' result <- rafsi_method(dataset, weights, criterion_type, ideal, anti_ideal, n_i, n_k)
#'
#' # View the result
#' print(result)
#'
#' @export
rafsi_method <- function(dataset, weights, criterion_type, ideal = numeric(), anti_ideal = numeric(), n_i, n_k) {

  # Check the sum of the weights
  if (abs(sum(weights) - 1) > .Machine$double.eps^0.5) {
    stop("The sum of the weights must be equal to 1.")
  }

  # Check the criterion types
  if (!all(criterion_type %in% c('max', 'min'))) {
    stop("Only 'min' or 'max' are valid for criterion_type.")
  }

  # Data normalization
  X <- dataset
  coef <- matrix(0, nrow = 2, ncol = ncol(X))
  best <- numeric(ncol(X))
  worst <- numeric(ncol(X))

  # Standardized decision matrix
  Standardized_matrix <- matrix(0, nrow = nrow(X), ncol = ncol(X))
  rownames(Standardized_matrix) <- rownames(dataset)

  for (j in seq_len(ncol(X))) {
    if (criterion_type[j] == 'max') {
      best[j] <- if (length(ideal) == 0) max(X[, j]) else ideal[j]
      worst[j] <- if (length(anti_ideal) == 0) min(X[, j]) else anti_ideal[j]
    } else {
      best[j] <- if (length(anti_ideal) == 0) min(X[, j]) else anti_ideal[j]
      worst[j] <- if (length(ideal) == 0) max(X[, j]) else ideal[j]
    }
    coef[1, j] <- (n_k - n_i) / (best[j] - worst[j])
    coef[2, j] <- (best[j] * n_i - worst[j] * n_k) / (best[j] - worst[j])

    for (i in seq_len(nrow(X))) {
      Standardized_matrix[i, j] <- X[i, j] * coef[1, j] + coef[2, j]
    }
  }

  # Normalize the decision matrix
  A <- mean(c(n_i, n_k))
  H <- 2 / (1 / n_i + 1 / n_k)

  Normalized_matrix <- Standardized_matrix

  for (j in seq_len(ncol(X))) {
    if (criterion_type[j] == 'max') {
      Normalized_matrix[, j] <- Standardized_matrix[, j] / (2 * A)
    } else {
      Normalized_matrix[, j] <- H / (2 * Standardized_matrix[, j])
    }
  }

  # Apply the weights
  Normalized_matrix_weighted <- sweep(Normalized_matrix, 2, weights, "*")
  rownames(Normalized_matrix_weighted) <- rownames(dataset)

  # Ranking as a dataframe with row names
  Ranking <- rowSums(Normalized_matrix_weighted)
  Ranking_df <- data.frame(
    Alternative = rownames(Normalized_matrix_weighted),
    Ranking = Ranking
  )
  rownames(Ranking_df) <- Ranking_df$Alternative

  # Sort the results
  sorted_indices <- order(Ranking, decreasing = TRUE)
  Ranking_df <- Ranking_df[sorted_indices, ]

  # Return the results as a list
  return(list(
    Standardized_matrix = Standardized_matrix,
    Normalized_matrix = Normalized_matrix,
    Ranking = Ranking_df
  ))
}

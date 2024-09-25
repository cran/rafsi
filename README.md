RAFSI: Ranking of Alternatives Through Functional Mapping of Criterion Sub-Intervals Into a Single Interval

This package provides an R implementation of the RAFSI method, which addresses the rank reversal problem in Multi-Attribute Decision-Making (MADM). Rank reversal occurs when unexpected changes in the ranking of alternatives happen due to the addition or deletion of a non-optimal alternative. This issue indicates significant mathematical inconsistencies in many MADM methods.

The RAFSI method is designed to produce stable rankings by mapping criterion sub-intervals into a single interval, ensuring that the final rankings remain consistent. The method returns a sorted ranking of alternatives based on a matrix of criteria values, where rows represent alternatives, and columns represent criteria.
Key Features:

    Criteria Matrix: A matrix where each row is an alternative, and each column represents a criterion.
    Criterion Weights: A numeric vector representing the weights for each criterion. The sum of these weights must equal 1.
    Ideal and Anti-Ideal Values: Numeric vectors representing the ideal and anti-ideal values for each criterion.
    Preference Relations: Numeric values n_i and n_k, which define the extent to which the ideal value is preferred over the anti-ideal value and the extent to which the anti-ideal value is considered the worst.

The function processes this data to generate a standardized decision matrix, normalizes it, applies the given weights, and returns the final sorted rankings.

More information about the method can be found at https://doi.org/10.3390/math8061015. 
More information about the implementation at https://github.com/mateusvanzetta/rafsi.

Installation:

Option 1: From CRAN

install.packages("rafsi")
library(rafsi)

Option 2: From GitHub


# Install devtools if you haven't already
install.packages("devtools")

# Install the latest version of rrafsir from GitHub
devtools::install_github("mateusvanzetta/rafsi")

Example Calculation Using Data from DOI Reference

Here’s how you can use the RAFSI method with sample data:


library(rafsi)

# Example dataset
dataset <- matrix(c(
  180, 165, 160, 170, 185, 167,
  10.5, 9.2, 8.8, 9.5, 10, 8.9,
  15.5, 16.5, 14, 16, 14.5, 15.1,
  160, 131, 125, 135, 143, 140,
  3.7, 5, 4.5, 3.4, 4.3, 4.1
), nrow = 6, ncol = 5)
rownames(dataset) <- c("A1", "A2", "A3", "A4", "A5", "A6")

# Define weights and criterion types
weights <- c(0.35, 0.25, 0.15, 0.15, 0.10)
criterion_type <- c('max', 'max', 'min', 'min', 'max')
ideal <- c(200, 12, 10, 100, 8)
anti_ideal <- c(120, 6, 20, 200, 2)
n_i <- 1
n_k <- 6

# Apply the RAFSI method
result <- rafsi_method(dataset, weights, criterion_type, ideal, anti_ideal, n_i, n_k)

# Print results
print(result$Standardized_matrix)
print(result$Normalized_matrix)
print(result$Ranking)

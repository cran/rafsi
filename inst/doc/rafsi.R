## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rafsi)

# Define the dataset (rows: alternatives, columns: criteria)
dataset <- matrix(c(
  180, 165, 160, 170, 185, 167,   # Criterion 1: Higher is better
  10.5, 9.2, 8.8, 9.5, 10, 8.9,   # Criterion 2: Lower is better
  15.5, 16.5, 14, 16, 14.5, 15.1, # Criterion 3: Lower is better
  160, 131, 125, 135, 143, 140,   # Criterion 4: Higher is better
  3.7, 5, 4.5, 3.4, 4.3, 4.1      # Criterion 5: Higher is better
), nrow = 6, ncol = 5, byrow = TRUE)

# Set names for the alternatives (A1 to A6)
rownames(dataset) <- c("A1", "A2", "A3", "A4", "A5", "A6")

# Define the weights for each criterion
weights <- c(0.35, 0.25, 0.15, 0.15, 0.10)

# Define the type of each criterion: 'max' for benefit, 'min' for cost
criterion_type <- c('max', 'min', 'min', 'max', 'max')

# Define the ideal values (best-case scenario) for each criterion
ideal <- c(200, 6, 10, 200, 8)

# Define the anti-ideal values (worst-case scenario) for each criterion
anti_ideal <- c(120, 12, 20, 100, 2)

# Number of criteria (n_i) and number of alternatives (n_k)
n_i <- 1
n_k <- 6

# Apply the RAFSI method
result <- rafsi_method(dataset, weights, criterion_type, ideal, anti_ideal, n_i, n_k)

# View the results
print(result)



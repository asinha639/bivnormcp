# Generating synthetic data
test_data <- data.frame(
  var1 = c(rnorm(50, 0, 0.5), rnorm(25, 5, 0.5), rnorm(25, 15, 0.5)),
  var2 = c(rnorm(50, 0, 0.5), rnorm(25, 5, 0.5), rnorm(25, 15, 0.5))
)

# Save the synthetic input data to CSV
write.csv(test_data, "example_runs/test_data.csv", row.names = FALSE)

# Load the package (install if needed)
# devtools::install_github("asinha639/bivnormcp")
library(bivnormcp)

# Run the change point detection function
result <- bivar_norm_cp(test_data$var1, test_data$var2,
                        th_cp = 0.9,
                        save_output = TRUE, file_name = "test_result.xlsx")

# Extract change points from result
change_pts <- index_correction(result$prob_max[, 2])

# Plot the detected change points in 3D
plot_cp_3d(result$x1, result$x2, change_pts)

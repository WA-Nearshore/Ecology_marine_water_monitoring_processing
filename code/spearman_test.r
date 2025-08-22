# Sample data (replace with your actual data)
my_data <- data.frame(
  variable_A = c(10, 12, 15, 18, 20),
  variable_B = c(5, 7, 9, 11, 13)
)

# Perform Spearman correlation test
spearman_result <- cor.test(my_data$variable_A, my_data$variable_B, method = "spearman")

# Print the results
print(spearman_result)

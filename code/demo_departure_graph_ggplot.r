# Load necessary libraries
library(ggplot2)
library(dplyr)

# Sample Data (using mtcars for demonstration)
data(mtcars)
df <- mtcars %>% select(wt, mpg)

# Calculate the overall mean of the y-variable (mpg)
mean_mpg <- mean(df$mpg)

# Create the plot
p <-ggplot(df, aes(x = wt, y = mpg)) +
  # Add the points (actual data)
  geom_point() +
  # Add a horizontal line for the overall mean
  geom_hline(yintercept = mean_mpg, color = "red", linetype = "dashed") +
  # Add vertical lines from the actual 'y' value to the mean 'y' value
  geom_segment(aes(x = wt, xend = wt, y = mean_mpg, yend = mpg),
               color = "blue",
               linetype = "solid",
               alpha = 0.6) +
  # Add labels and title
  labs(title = "Departure from Mean MPG at Each WT Value",
       x = "Weight (wt)",
       y = "Miles Per Gallon (mpg)") +
  # Optional: use a theme
  theme_minimal()

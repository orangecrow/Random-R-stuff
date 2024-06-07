# Install ggplot2 package if not available
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Define the function (example: f(x) = x^2)
f <- function(x) x^2

# Define the x-axis range for the area of interest
x_vals <- seq(from = -2, to = 2, length = 100)

# Calculate y-axis values for the curve
y_vals <- f(x_vals)

# Create a data frame with x and y values
data_for_plot <- data.frame(x = x_vals, y = y_vals)

# Create ggplot with geom_area for shaded area
ggplot(data = data_for_plot, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +  # Shaded area properties
  geom_line(aes(y = f(x)), col = "blue") +  # Line for the curve (corrected)
  labs(title = "Area under the Curve (f(x) = x^2)", x = "x-axis", y = "y-axis")


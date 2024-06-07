# Define the function (example: f(x) = x^2)
f <- function(x) x^2

# Define the x-axis range for the area of interest
x_vals <- seq(from = -2, to = 2, length = 100)

# Calculate y-axis values for the curve
y_vals <- f(x_vals)

# Calculate y-axis at x=0 (for polygon closure)
y_at_zero <- f(0)

# Combine x and y values for the polygon
polygon_points <- c(x_vals, rev(x_vals), rep(0, length(x_vals)), y_at_zero)

# Plot the curve and shaded area
plot(x_vals, y_vals, type = "l", col = "blue")  # Line for the curve
polygon(polygon_points, col = "lightblue", lwd = 2)  # Shaded area

# Add labels and title (optional)
xlabel("x-axis")
ylabel("y-axis")
title("Area under the Curve (f(x) = x^2)")


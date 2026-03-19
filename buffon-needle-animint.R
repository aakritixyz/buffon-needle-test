library(animint2)
library(data.table)

set.seed(123)
n_drops <- 100
L <- 0.8  # Needle length
D <- 1.0  # Distance between lines (L < D)
total_lines <- 5

y_center <- runif(n_drops, 0, total_lines) 
theta <- runif(n_drops, 0, pi)
x_center <- runif(n_drops, 0, 10)

dy <- (L/2) * sin(theta)
dx <- (L/2) * cos(theta)

# Coordinates
y1_all <- y_center - dy
y2_all <- y_center + dy
x1_all <- x_center - dx
x2_all <- x_center + dx

is_cross <- floor(y1_all) != floor(y2_all)

# Statistics
cumulative_hits <- cumsum(is_cross)
iterations <- 1:n_drops
pi_estimates <- (2 * L * iterations) / (D * cumulative_hits)
pi_estimates[is.infinite(pi_estimates)] <- NA # Clean up early div by zero

# Table for the estimation plot
estimate_dt <- data.table(
  iteration = iterations,
  estimate = pi_estimates,
  hits = cumulative_hits
)

# Table for the needles (mapped to each iteration for persistence)
needle_dt <- rbindlist(lapply(iterations, function(i) {
  data.table(
    iteration = i,
    x1 = x1_all[1:i],
    x2 = x2_all[1:i],
    y1 = y1_all[1:i],
    y2 = y2_all[1:i],
    cross = is_cross[1:i]
  )
}))

# Plot 1
floor_plot <- ggplot() +
  geom_hline(yintercept = 0:total_lines, color = "black", size = 1) + 
  geom_segment(data = needle_dt,
               aes(x = x1, xend = x2, y = y1, yend = y2, color = cross),
               showSelected = "iteration") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
  geom_text(data = estimate_dt,
            aes(x = 5, y = total_lines + 0.5, 
                label = sprintf("Drops: %d | Hits: %d | Pi Estimate: %.4f", iteration, hits, estimate)),
            showSelected = "iteration", size = 5) +
  labs(title = "Buffon's Needle Simulation", x = "", y = "") +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "none", panel.grid = element_blank())

# Plot 2

conv_plot <- ggplot() +
  geom_hline(yintercept = pi, linetype = "dashed", color = "blue") +
  geom_line(data = estimate_dt, aes(x = iteration, y = estimate), color = "black") +
  geom_point(data = estimate_dt, aes(x = iteration, y = estimate),
             showSelected = "iteration", clickSelects = "iteration",
             color = "red", size = 3) +
  labs(title = "Convergence to Pi", x = "Number of Drops", y = "Estimate") +
  theme_minimal()

viz <- animint(
  floor = floor_plot,
  convergence = conv_plot,
  time = list(variable = "iteration", ms = 400),
  duration = list(iteration = 0), # No smoothing for better performance
  title = "Buffon's Needle Pi Approximation"
)

animint2dir(viz, out.dir = "buffon-needle-test", open.browser = TRUE)
library(dplyr)
library(ggplot2)
library(purrr)

###### Question 1


data <- read.csv("Macro project 2.csv")

#pulling out GDP, Captial stock, and total workforce     ALL IN 2017 BASE!!!
GDP_data_UK <- df$rgdpna
Capitalstock_data_UK <- df$rnna
Workforce_data_UK <- df$emp


#rearranged to find log(At)

log(At) = log(Yt) - alpha * log(Kt) - (1-alpha) * log(Lt)

alpha <- 0.33

# Computing At

df <- df %>%
  mutate(log_At = log(GDP_data_UK) - alpha * log(Capitalstock_data_UK) - (1 - alpha) * log(Workforce_data_UK),
         At = exp(log_At))

log_At <- df$log_At
At <- df$At


#Setting base year 1950 since previous variables in 2017

base_year <- df$year[1]
A_base <- df$At[df$year == base_year]

# Normalize A_t
df <- df %>%
  mutate(normalized_1950 = At / A_base)


#plot it

ggplot(df, aes(x = year, y = normalized_1950)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", linewidth = 2) +
  labs(title = "Solow Residual (At) Over Time",
       x = "Year",
       y = "Total Factor Productivity (At)") +
  theme_minimal()



######### Question 2

alpha <- 0.33  # Capital elasticity
delta <- 0.07  # Depreciation rate
s <- 0.1010     # Savings rate in the UK in 2017
A_t <- df$normalized_1950   # Solow residual
k0 <- df$rnna[1] / (df$emp[1])  # Initial capital per worker (K / L)


###################
GDP_data_UK <- data$rgdpna
Capitalstock_data_UK <- data$rnna
Workforce_data_UK <- data$emp

alpha <- 0.33

data <- data %>%
  mutate(log_At = log(GDP_data_UK) - (alpha * log(Capitalstock_data_UK)) - ((1 - alpha) * log(Workforce_data_UK)),
         At = exp(log_At))


# Load necessary libraries
library(ggplot2)

# Assuming 'data' is already loaded as a data frame
data$year <- as.numeric(data$year)  # Convert Year to numeric

# Extract necessary variables
years <- data$year
A_t <- data$At  # Solow Residual
K_t <- Capitalstock_data_UK  # Capital Stock
L_t <- Workforce_data_UK  # Labor Force

# Parameters
s <- 0.25  # Savings rate (29.27%)
delta <- 0.05  # Depreciation rate (7%)
alpha <- 0.33  # Capital elasticity

# Set base year to 1991
base_year <- 1950
base_index <- which(years == base_year)[1]

# Compute initial capital per worker
k0 <- K_t[base_index] / L_t[base_index]

# Time periods for simulation
t_max <- length(years)
k_t <- numeric(t_max)
k_t[1] <- k0

# Solow growth function
solow_growth <- function(k, s, A, alpha, delta) {
  return(s * A * k^alpha + (1 - delta) * k)
}

# Compute path of k_t using actual A_t values
for (t in 2:t_max) {
  k_t[t] <- solow_growth(k_t[t - 1], s, A_t[t], alpha, delta)
  
  # Check for steady state (convergence)
  if (abs(k_t[t] - k_t[t - 1]) < 1e-3) {
    print(paste("Steady state reached at period", years[t], "with k* =", round(k_t[t], 2)))
    k_t <- k_t[1:t]  # Trim the array to the converged period
    years <- years[1:t]
    break
  }
}

# Debugging print statements
print(paste("First few values of k_t:", toString(head(k_t, 5))))
print(paste("Final value of k_t (steady state):", k_t[length(k_t)]))

# Plot results
ggplot(data.frame(years, k_t), aes(x = years, y = k_t)) +
  geom_line(color = 'blue', size = 1) +
  geom_hline(yintercept = k_t[length(k_t)], color = 'red', linetype = "dashed") +
  labs(
    title = "Capital per Worker in UK",
    x = "Year",
    y = "Capital per Worker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "topleft"
  ) +
  scale_y_continuous(labels = scales::comma)

########################
#sim parameters
T <- 70  # Number of periods to simulate
k <- numeric(T)  # Create a vector to store capital per worker
k[1] <- k0       # Initial capital per worker


# simulation

for (t in 1:(T-1)) {
  k[t+1] <- s * A_t[t] * k[t]^alpha + (1 - delta) * k[t]
}


#Plot it
plot(1:T, k, type = "l", col = "blue", lwd = 2,
     xlab = "Time (Years)", ylab = "Capital per Worker",
     main = "Capital per Worker")


#calculate log(Yt) for all years

log_Yt_calculated <- alpha * log(Capitalstock_data_UK) + (1-alpha) * log(Workforce_data_UK) + log_At

log_GDP_UK <- log(GDP_data_UK)

#compare in plot

ymin <- min(c(log_Yt_calculated, log_GDP_UK), na.rm = TRUE)
ymax <- max(c(log_Yt_calculated, log_GDP_UK), na.rm = TRUE)


plot(df$year, log_Yt_calculated, type = "l", col = "blue", lwd = 2, 
     xlab = "Year", ylab = "Output", 
     main = "Calculated Output vs Actual GDP",
     ylim = c(ymin, ymax))
lines(df$year, log_GDP_UK, col = "red", lwd = 2)
legend("topleft", legend = c("Calculated Output", "Actual GDP"),
       col = c("blue", "red"), lwd = 2)



# Question 3

alpha <- 0.33                # Capital elasticity
savings_rate <- 0.25         # Assumed savings rate
delta <- 0.05                # Depreciation rate
population_growth <- 0.0047  # Population growth rate

# Create a vector for output (Yt) and steady-state output (Yt_steady)
Yt_simulated <- numeric(length(df$year))
Yt_steady <- numeric(length(df$year))

# Loop through the years to simulate the path of output
for (t in 1:length(df$year)) {
  # Calculate capital per worker (kt) and steady-state capital (k_t_steady)
  k_t_steady <- (savings_rate * df$normalized_1950[t] / (delta + population_growth))^(1/(1-alpha))
  
  # Calculate output using Solow model (Yt)
  Yt_simulated[t] <- df$normalized_1950[t] * k_t_steady^alpha * Workforce_data_UK[t]^(1-alpha)
  
  # Calculate steady-state output (Yt_steady)
  Yt_steady[t] <- df$normalized_1950[t] * k_t_steady^alpha
}

# Plot the comparison of simulated output and actual GDP

base_year_index <- which(df$year == 1950)

Yt_simulated_norm <- Yt_simulated / Yt_simulated[base_year_index]
GDP_data_UK_norm <- GDP_data_UK / GDP_data_UK[base_year_index]

ymin <- min(c(Yt_simulated, GDP_data_UK), na.rm = TRUE)
ymax <- max(c(Yt_simulated, GDP_data_UK), na.rm = TRUE)


plot(df$year, Yt_simulated_norm, type="l", col="blue", lwd=2, xlab="Year", ylab="Normalized Output", 
     main="Simulated Output vs Actual GDP (Normalized)")
lines(df$year, GDP_data_UK_norm, col="red", lwd=2)
legend("topleft", legend=c("Simulated Output", "Actual GDP"), col=c("blue", "red"), lwd=2)



##### 4


library(nloptr)

# Define parameters
w1 <- 100
beta <- 0.95
r <- 0.05

# Define the utility function to maximize
utility <- function(x) {
  c1 <- x[1]  # Consumption in period 1
  c2 <- x[2]  # Consumption in period 2
  return(- (log(c1) + beta * log(c2)))  # Negative because we minimize in nloptr
}

# Constraints function
constraint <- function(x) {
  c1 <- x[1]
  c2 <- x[2]
  s <- w1 - c1
  return(c2 - s * (1 + r))  # This should be 0 to satisfy the budget constraint
}

# Initial guesses
x0 <- c(50, 50)

# Solve using nloptr
result <- nloptr(
  x0 = x0,
  eval_f = utility,
  eval_g_eq = constraint,
  lb = c(0, 0),  # Consumption can't be negative
  ub = c(w1, w1 * (1 + r)),  # Upper bounds
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8)
)

# Extract optimized values
c1_opt <- result$solution[1]
c2_opt <- result$solution[2]


# Print results
cat("Numerical Solution:\n")
cat("c1 =", c1_opt, "\n")
cat("c2 =", c2_opt, "\n\n")


######same everything with larger beta

# Define parameters
w1 <- 100
beta <- 1.5
r <- 0.05

# Define the utility function to maximize
utility <- function(x) {
  c1 <- x[1]  # Consumption in period 1
  c2 <- x[2]  # Consumption in period 2
  return(- (log(c1) + beta * log(c2)))  # Negative because we minimize in nloptr
}

# Constraints function
constraint <- function(x) {
  c1 <- x[1]
  c2 <- x[2]
  s <- w1 - c1
  return(c2 - s * (1 + r))  # This should be 0 to satisfy the budget constraint
}

# Initial guesses
x0 <- c(50, 50)

# Solve using nloptr
result <- nloptr(
  x0 = x0,
  eval_f = utility,
  eval_g_eq = constraint,
  lb = c(0, 0),  # Consumption can't be negative
  ub = c(w1, w1 * (1 + r)),  # Upper bounds
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8)
)

# Extract optimized values
c1_opt <- result$solution[1]
c2_opt <- result$solution[2]


# Print results
cat("Numerical Solution:\n")
cat("c1 =", c1_opt, "\n")
cat("c2 =", c2_opt, "\n\n")


######same everything with larger r

# Define parameters
w1 <- 100
beta <- 0.95
r <- 0.1

# Define the utility function to maximize
utility <- function(x) {
  c1 <- x[1]  # Consumption in period 1
  c2 <- x[2]  # Consumption in period 2
  return(- (log(c1) + beta * log(c2)))  # Negative because we minimize in nloptr
}

# Constraints function
constraint <- function(x) {
  c1 <- x[1]
  c2 <- x[2]
  s <- w1 - c1
  return(c2 - s * (1 + r))  # This should be 0 to satisfy the budget constraint
}

# Initial guesses
x0 <- c(50, 50)

# Solve using nloptr
result <- nloptr(
  x0 = x0,
  eval_f = utility,
  eval_g_eq = constraint,
  lb = c(0, 0),  # Consumption can't be negative
  ub = c(w1, w1 * (1 + r)),  # Upper bounds
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8)
)

# Extract optimized values
c1_opt <- result$solution[1]
c2_opt <- result$solution[2]


# Print results
cat("Numerical Solution:\n")
cat("c1 =", c1_opt, "\n")
cat("c2 =", c2_opt, "\n\n")




##### everything the same as first run but including w2=100

beta <- 0.95  # Discount factor
r <- 0.05     # Interest rate
w1 <- 100     # Initial wealth in period 1
w2 <- 100     # Initial wealth in period 2

# Utility function to maximize (we minimize -U for nloptr)
utility_function <- function(x) {
  c1 <- x[1]
  s <- x[2]
  c2 <- s * (1 + r) + w2
  if (c1 <= 0 || c2 <= 0) return(Inf)  # Avoid log of negative numbers
  return(- (log(c1) + beta * log(c2)))  # Minimize negative utility
}

# Constraint function
constraint_function <- function(x) {
  c1 <- x[1]
  s <- x[2]
  c2 <- s * (1 + r) + w2
  return(c(w1 - c1 - s, c2))  # Budget constraint should be 0
}

# Initial guess for c1 and s
x0 <- c(w1 / 2, w1 / 2)

# Bounds for variables (c1 > 0, s can be negative for borrowing)
lower_bounds <- c(1e-5, -Inf)
upper_bounds <- c(w1, Inf)

# Solve using nloptr
result <- nloptr(x0 = x0,
                 eval_f = utility_function,
                 lb = lower_bounds,
                 ub = upper_bounds,
                 eval_g_eq = constraint_function,
                 opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-8))

# Extract optimized values
c1_opt <- result$solution[1]
s_opt <- result$solution[2]
c2_opt <- s_opt * (1 + r) + w2

# Print results
cat("Numerical Optimization Results:\n")
cat("Optimal c1:", c1_opt, "\n")
cat("Optimal c2:", c2_opt, "\n")
cat("Optimal savings (s):", s_opt, "\n")



##### everything the same as first run but including w2=10000

beta <- 0.95  # Discount factor
r <- 0.05     # Interest rate
w1 <- 100     # Initial wealth in period 1
w2 <- 10000     # Initial wealth in period 2

# Utility function to maximize (we minimize -U for nloptr)
utility_function <- function(x) {
  c1 <- x[1]
  s <- x[2]
  c2 <- s * (1 + r) + w2
  if (c1 <= 0 || c2 <= 0) return(Inf)  # Avoid log of negative numbers
  return(- (log(c1) + beta * log(c2)))  # Minimize negative utility
}

# Constraint function
constraint_function <- function(x) {
  c1 <- x[1]
  s <- x[2]
  c2 <- s * (1 + r) + w2
  return(c(w1 - c1 - s, c2))  # Budget constraint should be 0
}

# Initial guess for c1 and s
x0 <- c(w1 / 2, w1 / 2)

# Bounds for variables (c1 > 0, s can be negative for borrowing)
lower_bounds <- c(1e-5, -Inf)
upper_bounds <- c(w1, Inf)

# Solve using nloptr
result <- nloptr(x0 = x0,
                 eval_f = utility_function,
                 lb = lower_bounds,
                 ub = upper_bounds,
                 eval_g_eq = constraint_function,
                 opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-8))

# Extract optimized values
c1_opt <- result$solution[1]
s_opt <- result$solution[2]
c2_opt <- s_opt * (1 + r) + w2

# Print results
cat("Numerical Optimization Results:\n")
cat("Optimal c1:", c1_opt, "\n")
cat("Optimal c2:", c2_opt, "\n")
cat("Optimal savings (s):", s_opt, "\n")


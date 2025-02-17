# Macro Theory Project 2

This is the R code for Macro Theory Project 2. This project uses the Solow model to analyze the United Kingdom's long-term economic growth using the country's inputs.

## Requirments
The following packages need to be installed before running the script:
- dyplr
- ggplot2
- purrr
- nloptr

## Part 1
### Growth Decomposition and Solow Residual

1. Extract GDP, Capital Stock, and the total workforce from the CSV file
2. Rearrange to find log(A<sub>t</sub>)
3. Compute A<sub>t</sub>
4. Set the base year to 1950 since the previous variable is from 2017
5. Normalize A<sub>t</sub> and plotting it over time

## Part 2
### Model Calibration and Path of k<sub>t</sub>

1. Define variables such as Capital Elasticity, Depreciation rate, Savings Rate in the UK in 2017, and initial capital per worker
2. Extract necessary variables for the model, such as the Solow Residual, Capital Stock, and Labor Force.
3. Set parameters and base year 1991
4. Compute initial capital per worker and establish time periods for the simulation
5. Compute k<sub>t</sub> using actual A<sub>t</sub> values and check for steady state
6. Plot results
7. Calculate log(Y<sub>t</sub>) for all years and compare in plot

## Part 3
### Simulated Steady-State Output vs. Actual Output
1. Define variables such as Capital Elasticity, Depreciation rate, Savings Rate, and population growth rate
2. Create a vector for output (Y<sub>t</sub>) and steady-state output (Y<sub>tsteady</sub>)
3. Loop through the years to simulate the path of output
4. # Plot the comparison of simulated output and actual GDP

## Part 4
### Solving an Optimization Problem
1. Define parameters of inital wealth, beta, and interest rate
2. Define the utility function to maximize and the constraint function.
3. Set the initial guesses and solve using nloptr
4. Extract and print optimized values
5. Repeat steps with a larger beta
6. Repeat again but set initial wealth for period 2 the same as in period 1
7. Repeat again but set initial wealth for period 2 to a value of 10,000









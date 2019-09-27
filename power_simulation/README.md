# Comparison of the statistical power of G×E MR with that of proxy G×E MR in the simulation

## Our simulation mainly has 4 steps:

(1) Generate data

(2) Calculate interaction P-value in each simulated dataset

(3) Count the times when interaction P-value < 0.05 out of 1000 times simulation in each scenario

(4) Plot the power under each scenario in one figure

## Set enviromental variables

To generate data using Linux, we set the following environment variable:
```
export simulation_DATA="/path/to/simulation/data"
```

We run all R scripts in Windows by setting the folder containing simulated data in R (version 3.5.1) as below:  
```
Sys.setenv(Mysimulation="/path/to/simulation/results/")
```

## Run the code

To run the R code via Linux, use:
```
Rscript simulation_DATA/generate_data_earlylife.R
```
To run the R code via Windows, use:
```
source ('interactionP_earlylife.R')
```

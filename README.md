# sim4comp simulation study

Training and testing of different machine learning algorithms over different scenarios!

## Project

The project consists of the following two main scripts:

-   `gendata_function.R`: containing the DGP.

-   `main.R`: containing the data simulation, the models deployment and the results plotting.

## Prerequisites

-   R software (recent version, I used R 4.5.2)

-   RStudio

## How to run locally

1.  Clone the repository;

2.  Open `sim4comp.Rproj` in RStudio;

3.  Run `renv::restore()` in the RStudio Console to install the packages;

4.  Run `main.R` first and `charts.R` later or, alternatively, just `charts.R` using the data already available in `final.results`.

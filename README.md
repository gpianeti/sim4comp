# sim4comp simulation study

Training and testing different machine learning algorithms across multiple scenarios!

## Project structure

The project consists of the following three main scripts:

-   `gendata_function.R`: containing the DGP.

-   `main.R`: runs the data simulation and trains the models.

-   `charts.R`: generates the plots of the results.

## Prerequisites

-   R software (recent version, tested with R 4.5.2)

-   RStudio

## How to run locally

1.  Clone the repository;

2.  Open `sim4comp.Rproj` in RStudio;

3.  Run `renv::restore()` in the RStudio Console to install the  required packages;

4.  Run `main.R` first and then `charts.R`. Alternatively, you can run only `charts.R` using the data already available in `final.results`.

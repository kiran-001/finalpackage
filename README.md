# KNIMMAKBMI Package

## Overview
The `knimmakbmi` package provides a collection of R functions aimed at enhancing data processing and statistical analysis tasks. This package was developed as part of a final project requirement for the BMI-510 course. It includes functions for handling log-likelihood calculations, survival curve estimation, unscaling data, principal component approximation, standardizing names in data frames, determining minimum sample sizes, and interfacing with RedCap reports.

## Installation
To install the `KNIMMAKBMI` package, use the following command in R:

# Install devtools if not already installed
```
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
```

# Install KNIMMAKBMI from GitHub
```
devtools::install_github("kiran-001/knimmakbmi")
```
## Functions

### `logLikBernoulli(data)`
Calculates the parameter `p` that maximizes the log-likelihood for a given Bernoulli distribution.

### `survCurv(status, time)`
Generates and plots a survival curve `S(t)` from provided status and time data vectors.

### `unscale(x)`
Reverses the centering and scaling transformations applied to a vector.

### `pcApprox(x, npc)`
Returns an approximation of the data based on the specified number of principal components.

### `standardizeNames(data)`
Standardizes variable names in a tibble to "small_camel" case using `dplyr::rename_with` and `janitor::make_clean_names`.

### `minimumN(x1, x2)`
Calculates the minimum sample size required for a t-test with predefined power and significance level, handling one or two sample scenarios.

### `downloadRedcapReport(redcapTokenName, redcapUrl, redcapReportId)`
Fetches a RedCap report as a tibble using API details specified in the user's environment.

## Examples
You can find examples on how to use each function in the documentation provided within the package. Access these examples via:
```
?logLikBernoulli
?survCurv
?unscale
?pcApprox
?minimumN
?standardizeNames
```

## Development
This package was developed using `devtools` and `roxygen2` to document and maintain the NAMESPACE automatically. For testing, `testthat` was utilized.

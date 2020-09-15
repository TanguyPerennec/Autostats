
# Autostats

## Overview

The goal of autostats is to help you perform basics **medical
statistics** from a dataframe, such as :

1.  *Table 1* : percentage, median, mean and statistical test between
    groups (khi2, Fisher, student)
2.  *Logistic regression* : with automated multivariate variable
    selection

## Installation

``` r
# The package is only available from github :
# install.packages("devtools")
devtools::install_github("TanguyPerennec/Autostats")
```

## Getting started

``` r
library(autostats)
```

Autostats functions are :

  - “Table 1” which render an usual table 1 with basics descriptive
    statistics (median, mean and proportions) of 2 groups (define by a
    binary variable called “y”) from a dataframe (called “DF”). Use
    `Table1(DF,y)`

  - “Logistic regression” which render a matrix with the results (odds
    ratios, confidence intervals and p-values) of univariate logistic
    regression of all the variables provided from the dataframe, and the
    results of a multivariate logistic regression with automated
    variable selection. Use `reglog(DF,y)` to have the matrix results or
    just `multivariate_selection(DF,y)` to have the selection with
    details of the selection method. You can provide a different method
    (“backward”,“forward”,“stepwise”,“augmented”) and selection criteria
    (“deviance”, “AIC”, “BIC”).

## Related work

Autostats will be used in an simulation project of multivariate logistic
regression selection so as to provide a better understanding of
multivariate selection process.

-----

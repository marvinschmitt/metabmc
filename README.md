
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metabmc

<!-- badges: start -->
<!-- badges: end -->

The **metabmc** is R package enables users to estimate and visualize
posterior distribution of model probabilities (meta models) with
simulation, and predictive mixture distribution by combining simulation
and posterior model probability obtained from observed data. This could
enable user to correct overconfidence over specific model in the context
of Bayesian Model Comparison.

## Installation

You can install the development version of metabmc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("marvinschmitt/metauncertainty")
```

## How to use metabmc

``` r
library(metabmc)
```

We use brms and Stan as a backbone to enable use to fit complicated
Bayesian model with simple syntax. To estimate meta models and
predictive density, we use all-in-one function `metabmc`. We simply
input `brmsfit` objects along with a argument specifying the settings
for simulations.

``` r
metabmc_fit <- metabmc(fit1, fit2, fit3, n_sim=20)
```

Posterior model probabilities obtained from simulation can be visualized
as follow.

``` r
plot_simulated_pmp(metabmc_fit)
```

Visualization of estimated meta models can be obtained as follow.

``` r
plot_meta_model_density(metabmc_fit)
```

The visualization of estimated predictive mixture model with observed
posterior model probabilities can be obtained as follow.

``` r
plot_predictive_mixture(metabmc_fit)
```

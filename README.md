## Overview

The **UGqBayesR** package provides tools for fitting Bayesian regression models for continuous doubly bounded responses defined on the unit interval \(0, 1\), based on the **unit-gamma distribution**. The package supports regression structures for **conditional quantiles**, allowing flexible modeling bounded data.

Bayesian inference is performed using **Markov Chain Monte Carlo (MCMC)** algorithms implemented via the **nimble** package, which offers a flexible environment for specifying and fitting hierarchical Bayesian models.

In addition to model estimation, **UGqBayesR** includes tools for:

- model fitting and posterior estimation via MCMC,
- convergence assessment of the parameter estimates (trace plots, R-hat, effective sample size)

For more information about the model formulation and estimation, please see: Rocha, Éric O., Nobre, J. S., Azevedo, C. L. N., Farias, R. B. A. \& Santos-Neto, M. (2025). A Bayesian Approach for Unit-Gamma Regression through Reparameterization in the Mean and Quantiles. Revista Colombiana de Estadística, 48(3), 397–431. https://doi.org/10.15446/rce.v48n3.123477

## Installation

You can install the development version from GitHub with:

```{r}
# install.packages("devtools")
devtools::install_github("ericolrc/UGqBayesR", force = TRUE)
```

## Example: Model Fitting and Convergence Diagnostics

This section illustrates how to fit a Bayesian unit-gamma quantile regression model using **UGqBayesR** and how to assess the convergence of the MCMC estimates.  
The example is based on the **Better Life Index (BLI)** dataset from **OECD countries**, as analyzed in the associated paper.

In this study, the response variable is the **educational attainment index** $y$, and the explanatory variables are **labor market insecurity (LMI)** $x_1$ and **homicide rate (HR)** $x_2$.

### Load packages

```{r}
library(UGqBayesR)
library(nimble)
library(coda)
library(bayesplot)
library(tidyverse)

y <- c(80,85,75,91,65,93,81,89,88,78,86,
       72,83,78,80,87,60,94,87,89,79,37,77,77,
       82,91,47,92,87,58,83,87,39,81,90,49,95,
       43)/100

x1 <- c(4.3,2.7,4.8,3.9,8.1,1.8,2.3,4,2.7,5,
        2,17.4,4.8,2.6,2.1,2.6,8.1,1.5,2.4,
        6.8,3.2,4.6,2.1,4.9,2.7,4.3,6.5,6.7,
        4,17.3,5.7,1.8,13,2.6,3.8,4.9,3.6,26.5)

x2 <- c(1,0.4,1,1.4,4.5,0.8,0.7,3.1,1.4,
        0.6,0.4,1,1.2,0.9,0.6,1.7,0.8,0.3,1.1,
        6.6,0.6,17.9,0.6,1.3,0.6,0.8,1,0.8,0.6,
        0.6,1,0.5,1.7,0.2,4.9,27.6,11.3,10)

df <- data.frame(y, x1, x2)

fam <- UGq_family(tau = 0.9)

fit <- UGq_fit(
  formula.mu    = y ~ x1 + x2,
  formula.sigma = y ~ x1 + x2,
  data          = df,
  family        = fam
)

summary(fit)
summary(fit, use.chains = "separate")

summary(fit, pars = "beta")   # regression coefficients for the quantile
summary(fit, pars = "gamma") # regression coefficients for the dispersion

diag <- UGq_diagnostics(fit)
diag

UGq_diagnostics(fit, plot = FALSE)
```


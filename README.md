simcausal
==========

<!-- [![Build Status](https://travis-ci.org/osofr/simcausal.png?branch=master)](https://travis-ci.org/osofr/simcausal) -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal) -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/simcausal)](http://cran.r-project.org/package=simcausal)
[![](http://cranlogs.r-pkg.org/badges/simcausal)](http://cran.rstudio.com/web/packages/simcausal/index.html)
[![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal)
[![Coverage Status](https://coveralls.io/repos/osofr/simcausal/badge.svg?branch=master&service=github)](https://coveralls.io/github/osofr/simcausal?branch=master)

The `simcausal` R package is a tool for specification and simulation of complex longitudinal data structures that are based on structural equation models (SEMs). The emphasis is on the types of simulations frequently encountered in causal inference problems, such as, observational data with time-dependent confounding, selection bias, and random monitoring processes. The interface allows for quick expression of dependencies between a large number of time-varying nodes. 

For example, to simulate data at a single time-point specified by 4 equations, use successive calls to `+ node` function and then `sim` function:

```R
library(simcausal)
D <- DAG.empty() + 
  node("CVD", distr="rcategor.int", probs = c(0.5, 0.25, 0.25)) +
  node("A1C", distr="rnorm", mean = 5 + (CVD > 1)*10 + (CVD > 2)*5) +
  node("TI", distr="rbern", prob = plogis(-0.5 - 0.3*CVD + 0.2*A1C)) +
  node("Y", distr="rbern", prob = plogis(-3 + 1.2*TI + 0.1*CVD + 0.3*A1C))
D <- set.DAG(D)
dat <- sim(D,n=200)
```

To display the above SEM object as a directed acyclic graph:

```R
plotDAG(D)
```

To allow the above nodes `A1C`, `TI` and `Y` to change over time, for time points t = 0,...,7, and keeping `CVD` the same, simply add `t` argument to `node` function and use the square bracket `[]` vector indexing to reference time-varying nodes inside the `node` function formulas:

```R
library(simcausal)
D <- DAG.empty() + 
  node("CVD", distr="rcategor.int", probs = c(0.5, 0.25, 0.25)) +
  node("A1C", t=0, distr="rnorm", mean=5 + (CVD > 1)*10 + (CVD > 2)*5) + 
  node("TI", t=0, distr="rbern", prob=plogis(-5 - 0.3*CVD + 0.5*A1C[t])) +

  node("A1C", t=1:7, distr="rnorm", mean=-TI[t-1]*10 + 5 + (CVD > 1)*10 + (CVD > 2)*5) +
  node("TI", t=1:7, distr="rbern", prob=plogis(-5 - 0.3*CVD + 0.5*A1C[t] + 1.5*TI[t-1])) +
  node("Y", t=0:7, distr="rbern", prob=plogis(-6 - 1.2*TI[t] + 0.1*CVD + 0.3*A1C[t]), EFU=TRUE)
D <- set.DAG(D)
dat.long <- sim(D,n=200)
```

The package also allows specification and simulation of counterfactual data under various user-specified interventions (e.g., static, dynamic, deterministic, or stochastic). In particular, the interventions may represent exposures to treatment regimens, the occurrence or non-occurrence of right-censoring events, or of clinical monitoring events. 

Finally, `simcausal` enables the computation of a selected set of user-specified features of the distribution of the counterfactual data that represent common causal quantities of interest, such as, treatment-specific means, the average treatment effects and coefficients from working marginal structural models. 


### Installation

To install the CRAN release version of `simcausal`: 

```R
install.packages('simcausal')
```

To install the development version (requires the `devtools` package):

```R
devtools::install_github('osofr/simcausal', build_vignettes = FALSE)
```

### Documentation

Once the package is installed, see the [vignette](http://cran.r-project.org/web/packages/simcausal/vignettes/simcausal_vignette.pdf), consult the internal package documentation and examples. 

* To see the vignette in R:

```R
vignette("simcausal_vignette", package="simcausal")
```

* To see all available package documentation:

```R
?simcausal
help(package = 'simcausal')
```

* To see the latest updates for the currently installed version of the package:

```{r, eval=FALSE}
news(package = "simcausal")
```

### Citation
To cite `simcausal` in publications, please use:
> Sofrygin O, van der Laan MJ, Neugebauer R (2015). *simcausal: Simulating Longitudinal Data with Causal Inference Applications.* R package version 0.1.

### Funding
The development of this package was partially funded through internal operational funds provided by the Kaiser Permanente Center for Effectiveness & Safety Research (CESR). This work was also partially supported through a Patient-Centered Outcomes Research Institute (PCORI) Award (ME-1403-12506) and an NIH grant (R01 AI074345-07).

### Copyright
This software is distributed under the GPL-2 license.

simcausal
==========

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/simcausal)](http://cran.r-project.org/package=simcausal)
[![](http://cranlogs.r-pkg.org/badges/simcausal)](http://cran.rstudio.com/web/packages/simcausal/index.html)
[![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal)
[![Coverage Status](https://coveralls.io/repos/osofr/simcausal/badge.svg?branch=master&service=github)](https://coveralls.io/github/osofr/simcausal?branch=master)

The `simcausal` R package is a tool for specification and simulation of complex longitudinal data structures that are based on structural equation models (SEMs). The emphasis is on the types of simulations frequently encountered in causal inference problems, such as, observational data with time-dependent confounding, selection bias, and random monitoring processes. The interface allows for quick expression of dependencies between a large number of time-varying nodes. 

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

```r
news(package = "simcausal")
```

### Brief overview

Below is an example simulating data with 4 covariates specified by 4 structural equations (nodes). New equations are added by using successive calls to `+ node()` function and data are simulated by calling `sim` function:

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

To allow the above nodes `A1C`, `TI` and `Y` to change over time, for time points t = 0,...,7, and keeping `CVD` the same, simply add `t` argument to `node` function and use the square bracket `[...]` vector indexing to reference time-varying nodes inside the `node` function expressions:

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

The `+ action` function allows defining counterfactual data under various interventions (e.g., static, dynamic, deterministic, or stochastic), which can be then simulated by calling `sim` function. In particular, the interventions may represent exposures to treatment regimens, the occurrence or non-occurrence of right-censoring events, or of clinical monitoring events.

In addition, the functions `set.targetE`, `set.targetMSM` and `eval.target` provide tools for defining and computing a few selected features of the distribution of the counterfactual data that represent common causal quantities of interest, such as, treatment-specific means, the average treatment effects and coefficients from working marginal structural models. 


### Using networks in SEMs

Function `network` provies support for networks simulations, in particular it enables defining and simulating SEM for dependent data. For example, a network sampling function like `generate.igraph.ER` defined below can be used to specify and simulate dependent data from a network-based SEM:

```R
#--------------------------------------------------------------------------------------------------
# Example of a network sampler that will supplied as "netfun" argument to network(, netfun=);
# Returns (n,Kmax) matrix of net IDs (friends) by row;
# Each row i will contain the IDs (row numbers) of observation i's friends;
#--------------------------------------------------------------------------------------------------
generate.igraph.ER <- function(n, m_pn, Kmax, ...) {
  m <- as.integer(m_pn[1]*n)
  if (n<=10) m <- 20
  igraph.ER <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
  sparse_AdjMat <- igraph.to.sparseAdjMat(igraph.ER)
  NetInd_out <- sparseAdjMat.to.NetInd(sparse_AdjMat)
  if (Kmax < NetInd_out$Kmax) message("new network has larger Kmax value than requested, new Kmax = " %+% NetInd_out$Kmax)
  return(NetInd_out$NetInd_k)
}
```

Next step is to start defining a SEM that uses the above network, with a `+network` syntax and providing `generate.igraph.ER` to `netfun` argument:

```R
Kmax <- 5
D <- DAG.empty()
# Define the network function and define its parameter(s) (m_pn):
D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generate.igraph.ER", m_pn = 1.4)
```

New nodes (structural equations) can now be specified conditional on the past node values of observations connected to each unit `i` (friends of `i`), where friends will be defined by the network ID matrix that will be returned by the `generate.igraph.ER` function above. Double square bracket syntax `[[...]]` now allows referencing the node values of connected friends as demonstrated in the example below:

```R
# W1 - categorical (5 categories, 0-4):
rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
D <- D + node("W1", distr = "rbinom2", size = 5, prob = c(0.4, 0.5, 0.7, 0.4))
# W2 - binary:
D <- D + node("W2", distr = "rbern", prob = 0.6)
# Define network variable netW1 as the W1 values of first friends across all observations:
D <- D + node("netW1", distr = "rconst", const = W1[[1]])
# Define exposure A[i] as a function of W1[i] and the total sum of i's friends values of W1 and W2:
D <- D + node("A", distr = "rbern",
              prob = plogis(2 + -0.5 * W1 +
                            -0.1 * sum(W1[[1:Kmax]]) +
                            -0.7 * sum(W2[[1:Kmax]])),
              replaceNAw0 = TRUE)
Dset <- set.DAG(D)

dat.net <- sim(Dset, n=100)
```

`Kmax` is a special variable that defines the maximum number of friends that any observation can have and it can be used for defining the network subsetting vector within `[[...]]`. When `kth` friend referenced by `Var[[k]]` doesn't exist, the default is to set that value to `NA` and adding the argument `replaceNAw0=TRUE` to `node` function changes such values from `NA` to `0`. Any summary measures that can be applied to multiple time-varying nodes can be similarly applied to network indexed nodes. For additional details, see the package documentation for the network function (`?network`).

### Citation
To cite `simcausal` in publications, please use:
> Sofrygin O, van der Laan MJ, Neugebauer R (2015). *simcausal: Simulating Longitudinal Data with Causal Inference Applications.* R package version 0.1.

### Funding
The development of this package was partially funded through internal operational funds provided by the Kaiser Permanente Center for Effectiveness & Safety Research (CESR). This work was also partially supported through a Patient-Centered Outcomes Research Institute (PCORI) Award (ME-1403-12506) and an NIH grant (R01 AI074345-07).

### Copyright
This software is distributed under the GPL-2 license.

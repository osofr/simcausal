simcausal
==========

<!-- [![Build Status](https://travis-ci.org/osofr/simcausal.png?branch=master)](https://travis-ci.org/osofr/simcausal) -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal) -->
[![](http://cranlogs.r-pkg.org/badges/simcausal)](http://cran.rstudio.com/web/packages/simcausal/index.html)
[![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal)
[![Coverage Status](https://coveralls.io/repos/osofr/simcausal/badge.png?branch=master)](https://coveralls.io/r/osofr/simcausal)

The `simcausal` R package is a tool for specification and simulation of complex longitudinal data structures that are based on structural equation models. The package aims to provide a flexible tool for simplifying the conduct of transparent and reproducible simulation studies, with a particular emphasis on the types of data and interventions frequently encountered in typical causal inference problems, such as, observational data with time-dependent confounding, selection bias, and random monitoring processes. 


The package interface allows for concise expression of complex functional dependencies between a large number of nodes, where each node may represent a time-varying random variable. The package allows for specification and simulation of counterfactual data under various user-specified interventions (e.g., static, dynamic, deterministic, or stochastic). In particular, the interventions may represent exposures to treatment regimens, the occurrence or non-occurrence of right-censoring events, or of clinical monitoring events. 


`simcausal` enables the computation of a selected set of user-specified features of the distribution of the counterfactual data that represent common causal quantities of interest, such as, treatment-specific means, the average treatment effects and coefficients from working marginal structural models. 


### Installation

`simcausal` can be installed from CRAN: 

```R
install.packages('simcausal')
```

To install the development version of `simcausal` (requires the `devtools` package):

```R
devtools::install_github('osofr/simcausal', build_vignettes = FALSE)
```

### Documentation

Once the package is installed, please read the [vignette](http://cran.r-project.org/web/packages/simcausal/vignettes/simcausal_vignette.pdf), consult the internal package documentation and examples. 

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
<!-- 
### Details

The \pkg{simcausal} R package is a comprehensive set of tools for specification and simulation of complex longitudinal data structures to study causal inference methodologies. The package is developed using the R system for statistical computing \citep{r} and is available from the Comprehensive R Archive Network (CRAN) at \url{http://CRAN.R-project.org/package=simcausal}. The main motivation behind the package is to provide a flexible tool to \emph{facilitate} the conduct of \emph{transparent} and \emph{reproducible} simulation
studies, with a particular emphasis on the types of data and interventions frequently encountered in real-world causal
inference problems. For example, the package simplifies the simulation of observational data based on random clinical
monitoring to evaluate the effect of time-varying interventions in the presence of time-dependent confounding and
sources of selection bias (e.g., informative right censoring). The package is built to provide a novel user-interface
that allows concise and intuitive expression of complex functional dependencies for a large number of nodes that may
represent time-varying random variables (e.g., repeated measurements over time of the same subject-matter attribute,
such as, blood pressure).


Each data generating distribution is specified via a structural equation model (SEM) \citep{pearl1995, Pearl2009,
pearl2010}. The package allows for specification and simulation of counterfactual data (referred to as ``full data'')
under various user-specified interventions (e.g., static, dynamic, deterministic, or stochastic), which are referred to
as ``actions''. These actions may represent exposure to treatment regimens, the occurrence or non-occurrence of right-
censoring events, or of clinical monitoring events (e.g., laboratory measurements based on which treatment decisions may
be made). Finally, the package enables the computation of a selected set of user-specified features of the distribution
of the full data that represent common causal quantities of interest, referred to as causal target parameters, such as,
treatment-specific means, the average treatment effects (ATE) (on the multiplicative or additive scale) and coefficients
from working marginal structural model (MSM) \citep{neugebauer2007}. We demonstrate an application of the
\pkg{simcausal} package by replicating the results of two published simulation studies from the causal inference
literature \citep{neugebauer2014,neugebauer2015,lefebvre2008}.


We note that the \pkg{simcausal} package differs from other R packages that implement data simulation based on
structural equation modeling in the following ways. First, \pkg{simcausal} does not restrict the set of distributions
available to the analyst to conduct a simulation study, i.e., any distribution that is currently available in
R or that can be user-defined in the R programming environment can be used to sample observations
in the \pkg{simcausal} package. In particular, the \pkg{simcausal} package is not restricted to data simulation based on
linear structural equations only. Thus, this package allows the analyst to specify arbitrary functional dependencies
between random variables, and, hence, enables data simulation from a much larger set of data generating mechanisms.
Second, the \pkg{simcausal} package introduces an intuitive user-interface for specifying complex data-generating
distributions to emulate realistic real-world longitudinal data studies characterized by a large number of repeated
measurements of the same subject-matter attributes over time. Third, this package is particularly tailored to conduct
data simulations to study causal inference methodologies for investigating the effect of complex intervention regimens
such as dynamic and stochastic interventions (not just the common static and deterministic intervention regimens), and
summary measures of these effects defined by (working) marginal structural models. The anticipated practical utility of
this package thus extends beyond methodological research purposes by providing a tool for simulation-based power
calculations to inform the design and analyses of real-world studies. Finally, the \pkg{simcausal} package provides a
pipeline for conducting the typical steps of most simulation studies that consists of defining the observed data
distribution, defining intervention/counterfactual distributions, defining causal parameters, simulating observed and
counterfactual data, and evaluating the true value of causal parameters.

### An Example
Following the specification of the distribution of the observed data (the \code{DAG} object), the user can set the actions (interventions) by changing the distribution for a subset of the \code{DAG} nodes and simulate data based on those actions ("\emph{full data}"). The package enables evaluation of various target causal parameters, such as the expectations of a subset of \code{DAG} nodes or parameters defined by coefficients of the working \emph{marginal structural model} (MSM).
... -->

### Citation
To cite `simcausal` in publications, please use:
> Sofrygin O, van der Laan MJ, Neugebauer R (2015). *simcausal: Simulating Longitudinal Data with Causal Inference Applications.* R package version 0.1.

### Funding
The development of this package was partially funded through internal operational funds provided by the Kaiser Permanente Center for Effectiveness & Safety Research (CESR). This work was also partially supported through a Patient-Centered Outcomes Research Institute (PCORI) Award (ME-1403-12506) and an NIH grant (R01 AI074345-07).

### Copyright
This software is distributed under the GPL-2 license.

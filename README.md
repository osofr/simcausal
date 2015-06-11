simcausal
==========

<!-- [![Build Status](https://travis-ci.org/osofr/simcausal.png?branch=master)](https://travis-ci.org/osofr/simcausal) -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal) -->
[![](http://cranlogs.r-pkg.org/badges/simcausal)](http://cran.rstudio.com/web/packages/simcausal/index.html)
[![Travis-CI Build Status](https://travis-ci.org/osofr/simcausal.svg?branch=master)](https://travis-ci.org/osofr/simcausal)
[![Coverage Status](https://coveralls.io/repos/osofr/simcausal/badge.png?branch=master)](https://coveralls.io/r/osofr/simcausal)

`simcausal` allow simulations of longitudinal data with causal inference applications.

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

Once the package is installed, please read the vignette, consult the internal package documentation and examples. 

* To see the package vignette:

```R
vignette("simcausal_vignette", package="simcausal")
```

* To see a list of all available package documentation:


```R
help(package = 'simcausal')
```

* To see the overview help file:

```R
?simcausal
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

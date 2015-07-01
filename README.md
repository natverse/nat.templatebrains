# nat.templatebrains
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat.templatebrains)](http://cran.r-project.org/web/packages/nat.templatebrains) 
[![Release Version](https://img.shields.io/github/release/jefferislab/nat.templatebrains.svg)](https://github.com/jefferislab/nat.templatebrains/releases/latest) 
[![Build Status](https://travis-ci.org/jefferislab/nat.templatebrains.svg)](https://travis-ci.org/jefferislab/nat.templatebrains)
[![Coverage Status](https://img.shields.io/coveralls/jefferislab/nat.templatebrains.svg)](https://coveralls.io/r/jefferislab/nat.templatebrains?branch=master)

## Quick Start

For the impatient ...

```r
# install
install.packages("nat.templatebrains")

# use
library(nat.templatebrains)

# basic examples for templatebrain class
example("as.templatebrain")
example("templatebrain-meths")

# run examples for mirroring
example("mirror_brain")
# ... and bridging
# NB you need to install nat.flybrains package to run some examples which are
# not run by default
example("xform_brain")

# get overview help for package
?nat.templatebrains
# help for main functions
?mirror_brain
?xform_brain
# if you want to construct your own templatebrains
?templatebrain

# run tests
library(testthat)
test_package("nat.templatebrains")
```

**nat.templatebrains** provides additional functions for use with the [NeuroAnatomy Toolbox](https://github.com/jefferis/nat) (nat). In particular, it defines the notion of a template brain, as used in image registration of 3D data, along with bridging registrations between template brains (see https://github.com/jefferislab/BridgingRegistrations) and mirroring registrations from one brain hemisphere to the other.

This is a generic package, usable with data from any species. For _Drosophila_ specific functions and data, see our [nat.flybrains](https://github.com/jefferislab/nat.flybrains) package. Installing/loading **nat.flybrains** will automatically
install/load **nat.templatebrains**.

## Installation
This package has now been released to CRAN (v0.4.1), but since this is the first official
release you may wish to install the development version from GitHub.

### CRAN release
```r
install.packages("nat.templatebrains")
```


### Bleeding Edge
You can install the development version by downloading the 
[tar ball](https://github.com/jefferislab/nat.templatebrains/tarball/master),
and running `R CMD INSTALL` on it, or (**recommended**) using the **devtools** package:

  ```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("nat.templatebrains", "jefferislab")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.

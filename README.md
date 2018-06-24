# nat.templatebrains
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat.templatebrains)](https://cran.r-project.org/package=nat.templatebrains) 
[![Release Version](https://img.shields.io/github/release/jefferislab/nat.templatebrains.svg)](https://github.com/jefferislab/nat.templatebrains/releases/latest) 
[![Build Status](https://travis-ci.org/jefferislab/nat.templatebrains.svg)](https://travis-ci.org/jefferislab/nat.templatebrains)
[![Coverage Status](https://img.shields.io/coveralls/jefferislab/nat.templatebrains.svg)](https://coveralls.io/r/jefferislab/nat.templatebrains?branch=master)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferislab.github.io/nat.templatebrains/reference/)

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
This package has now been released to CRAN (since v0.4.1), but since there are 
regular updates between CRAN releases we generally recommend that you install 
the development version from GitHub.

### CRAN release
```r
install.packages("nat.templatebrains")
```

### Development version
If you want to install the development version of nat.templatebrains, you can do
this using devtools. You will probably also wish to install the development
version of the nat package if you do this.

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferislab/nat")
devtools::install_github("jefferislab/nat.templatebrains")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) as 
well as [devtools](https://CRAN.R-project.org/package=devtools) to install this way.

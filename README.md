# R mrregression package

## Purpose and Functionality

Regression analysis for model parametrization

## Installation

For installation of the most recent package version an additional repository can be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrregression")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/mrregression.svg?branch=master)](https://travis-ci.com/pik-piam/mrregression)

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3699647.svg)](https://doi.org/10.5281/zenodo.3699647)



# Regression analysis based on global datasets

R package **regressionworlddata**, version **1.1.1**

[![CRAN status](https://www.r-pkg.org/badges/version/regressionworlddata)](https://cran.r-project.org/package=regressionworlddata)  [![Travis build status](https://travis-ci.com/pfuehrlich-pik/regressionworlddata.svg?branch=master)](https://travis-ci.com/pfuehrlich-pik/regressionworlddata)  

## Purpose and Functionality

Model estimates parameters of model functions.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("regressionworlddata")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/regressionworlddata.svg?branch=master)](https://travis-ci.com/pik-piam/mrregression)

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3699647.svg)](https://doi.org/10.5281/zenodo.3699647)

To cite package **regressionworlddata** in publications use:

Bodirsky B, Walther A, Wang X, Mishra A, Martinelli E (2021). _regressionworlddata: Regression analysis based on global
datasets_. R package version 1.1.1.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {regressionworlddata: Regression analysis based on global datasets},
  author = {Benjamin Leon Bodirsky and Antonia Walther and Xiaoxi Wang and Abhijeet Mishra and Eleonora Martinelli},
  year = {2021},
  note = {R package version 1.1.1},
}
```

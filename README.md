[![Travis-CI Build Status](https://travis-ci.org/dashaub/supervisedPRIM.svg?branch=master)](https://travis-ci.org/dashaub/supervisedPRIM)
[![Coverage Status](https://coveralls.io/repos/github/dashaub/supervisedPRIM/badge.svg?branch=master)](https://coveralls.io/github/dashaub/supervisedPRIM?branch=master)
[![CRAN version](http://www.r-pkg.org/badges/version/supervisedPRIM)](http://www.r-pkg.org/pkg/supervisedPRIM)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/supervisedPRIM)](http://www.r-pkg.org/pkg/supervisedPRIM)

# supervisedPRIM
Supervised classification learning and prediction using Patient Rules Induction Method

## Installation
The stable release of the package is hosted on [CRAN](https://cran.r-project.org/web/packages/supervisedPRIM/index.html) and can be installed as usual:
````r
install.packages("supervisedPRIM")
````

The latest development version can be installed using the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.
```r
devtools::install_github("dashaub/supervisedPRIM")
```
Version updates to CRAN will be published frequently after new features are implemented, so the development version is not recommended unless you plan to modify the code.

## Usage
The `supervisePRIM()` function can be used to train a model on a dataset of all numeric columns with a binary 0/1 response. For example, using the famous `iris` dataset
```
data(iris)
yData <- ifelse(iris$Species == "setosa", 1L, 0L)
xData <- iris
xData$Species <- NULL
primModel <- supervisedPRIM(x = xData, y = yData)
```
This returns a S3 class `supervisedPRIM` object, and the regular S3 `predict()` generic can be used to apply the model to new data:
```
predictions <- predict(primModel, newdata = xData)
```
Furthermore, this `supervisedPRIM` objects also inherits from the "prim" package, so all the regular method there (e.g. `plot()`) can be used on the `supervisedPRIM` objects. Consult the documention of the "prim" package for more comprehensive details of the available functions and the arguments accepted for training.

## License
This package is free software released under the [GPL-3](http://www.gnu.org/licenses/gpl-3.0.en.html) license.

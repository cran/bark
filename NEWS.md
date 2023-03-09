# bark 1.0.2


# bark 1.0.1

## minor changes

* update longer running examples to use `\donttest` rather than `\dontrun`
* add reference to DESCRIPTION

# bark 1.0.0

## Major Changes

* output of `bark` is now a `bark` object which will allow S3 methods

* replace input arguments `y.train` and `x.train` to use model formula and 
dataframe as inputs.

* replaced the argument `type` with two logical variables: `selection` and `common_lambdas` that are more intuitive.

* changed running times of examples to address issue  https://CRAN.R-project.org/package=bark which led package to being archived in 2015

* added registration of native routines in foreign function calls and disabled symbol search in `src/bark-init.c` and updated the `NAMESPACE`

* replaced kernel calculation using `.C` with `.Call` to improve speed in `src/kernelCalculationCall.cpp` and `R/llike.R`

* added unit tests in `testthat` so that code coverage is reported with CI; 
  code coverage  badge added to README.   Unit tests now cover over 99% of 
  the code as reported by CodeCov
  
* added GitHub actions for CI and checks on Windows, MacOSX, and Ubuntu and 
  added R CMD check passing Badge to README.md in GitHub repo.

* converted all help files to use `roxygen` tags

* deprecated functions `sim.Circle`, `sim.Friedman1`, `sim.Friedman2`, `sim.Friedman3` and created new versions  `sim_circle`, `sim_Friedman1`, `sim_Friedman2`, `sim_Friedman3` to avoid confusion with S3 methods

* updated CODE_OF_CONDUCT.md, SECURITY.md, CONTRIBUTING.md on GitHub repo
  and other updates for  OpenSSF BestPractices Badge (added to README.md)


## Bug Fixes


* GitHub Issue #1 Added type checks to `src/kernelCalculationCall.cpp` and coerce inputs to correct type  (unit tests in `testthat/test-llike.R`)  reported at 


* GitHub Issue #3  Addressed error in when `p = 1` where subsetting 
  resulted in output being a vector due to drop in dimension.    Added unit test
  in `testthat/test-bark.R`

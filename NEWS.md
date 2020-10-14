# bark 1.0.0

## Major Changes

* changed running times of examples to address issue  https://CRAN.R-project.org/package=bark which led package to being archived in 2015

* added registration of native routines in foreign function calls and disabled symbol search in `src/bark-init.c` and updated the `NAMESPACE`

* deprecated functions `sim.Circle`, `sim.Friedman1`, `sim.Friedman2`, `sim.Friedman3` and created new versions  `sim_circle`, `sim_Friedman1`, `sim_Friedman2`, `sim_Friedman3` to avoid confusion with S3 methods

* replaced kernel calculation using `.C` with `.Call` to improve speed in `src/kernelCalculationCall.cpp` and `R/llike.R`

* added unit test in `testthat`

* converted all help files to use `roxygen` tags

## Bugs

To Do 

* fix `src/kernelCalculationCall.cpp` (`testthat/test-llike.R`) 

* coerce inputs to correct type in `createDesign` in `R/llike.R` (#1)


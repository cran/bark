# bark 1.0.0. Comments to CRAN

Submission of archived package to fix running times
of examples and bring up to current standards

Updates
  - added unit tests and code coverage reports (99% code coverage)
  - added GitHub Actions for CI on multiple platforms
  - added registration of native routines in foreign function calls and disabled symbol search in `src/bark-init.c` and updated the `NAMESPACE` using Roxygen2
  - changed function arguments to use a model formula
  
## Test environments
- local R installation macosx, R 4.2.2
- ubuntu  (r-release, r-devel, and old-release)
- win-builder (r-release, r-devel)
- mac-builder-M1mac (r-release, r-devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub linux-x86_64-rocker-gcc-san (r-devel)


## R CMD check results

0 errors | 0 warnings | 1 note

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2015-01-15 as did not comply with
    policies on timings of examples.
    
## Reverse Dependencies

None    
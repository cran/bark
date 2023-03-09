# bark 1.0.1 Comments to CRAN

Re-submission of package that had been archived in 2015 due to running times
of examples not complying with CRAN policy

Comments 

- fixed running time of examples so that all are less than 5 seconds on test platforms (see below) by reducing the number of iterations for illustration.  Additional longer examples are in \donttest now.
- added reference to DESCRIPTION file
- added \value to the function  man page of `bark-package-deprecated` that is used to list all deprecated functions from the package (returns nothing)
- long running examples are enclosed in \donttest rather than \dontrun per reviewers request. Deprecated functions use \dontrun to avoid warnings.
- added all authors, contributors and copyright holders in the Authors@R field with the appropriate roles.  

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
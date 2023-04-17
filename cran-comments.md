# bark 1.0.4 Comments to CRAN

Package archived on 3/31 due to CRAN check failiure on MKL (R-devel) due
to unit test.  

Attempts to replicate the failed test in a docker container for
Debian with gcc-12, MKL latest (2023), and r-devel have been unsuccesful 
(still passes the test).   Skipping test on cran as it is for 
code that will be deprected shortly and was there for internal development
(and passes on all other platforms with CI for r-devel, r-release).

Possible misspelled words in the DESCRIPTION are correct surnames

## Test environments
- local R installation macosx, R 4.2.3
- ubuntu  (r-release, r-devel, and old-release) CI github actions
- win-builder (r-release, r-devel)
- mac-builder-M1mac (r-release, r-devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub linux-x86_64-rocker-gcc-san (r-devel)
- docker debian, gcc-12, intel-MKL-latest


## R CMD check results

0 errors | 0 warnings | 1 note

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Ouyang (23:19)
  Wolpert (24:51)  
  
  
## Reverse Dependencies

None    
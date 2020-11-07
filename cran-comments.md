## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)
* R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

##Resubmission

This is a re-submission after the package was archived on CRAN. The resubmission fixes CRAN comments. The changes made are: The quote of external packages were modified by the correct format, the /dontrun of the examples was modified by /donttest, the user's options in the nsgaMonitor function are restored to their original value after changing them with on.exit.

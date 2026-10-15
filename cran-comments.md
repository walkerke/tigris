## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS Tahoe 26.6 (aarch64), R 4.6.1
* win-builder (R-devel)

## Reverse dependencies

We checked 35 reverse dependencies, comparing R CMD check results across the CRAN and development versions of this package. We saw 0 new problems, and failed to check 0 packages.

## Notes

* Downloads now use httr2 in place of httr, which raises the R requirement to >= 4.1.
* Tests that download Census files are skipped on CRAN.

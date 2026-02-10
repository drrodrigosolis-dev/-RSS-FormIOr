## Test environments
* local macOS Tahoe 26.2, R 4.5.2 (aarch64-apple-darwin20)
  - `R CMD check --as-cran FormIOr_1.0.1.tar.gz`
  - `_R_CHECK_FORCE_SUGGESTS_=false` (no internet to install Suggests)

## R CMD check results
0 ERROR | 0 WARNING | 2 NOTEs

NOTEs:
* URL checks failed locally due to no internet access (libcurl could not resolve github.com or github.io).
* unable to verify current time (local environment)

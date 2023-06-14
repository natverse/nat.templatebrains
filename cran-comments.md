## Test environments
* local R installation, R 4.3.0
* ubuntu 20.04 (github actions), R 4.3.0
* win-builder (devel)
https://win-builder.r-project.org/xy6hBvBl0Ir4/00check.log

This release is principally to reverse the archival triggered by temporary 
archival of the strong dependency nat.utils (now reversed).
This release adds a small amount of new functionality to a relatively mature package.
A number of URLs in README/Description have been updated on the basis of
R CMD check forwarding tests from win-builder.

## R CMD check results

0 errors | 0 warnings | 1 notes

(pertaining to archival and a false positive proposed spelling issue)

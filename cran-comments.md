## Reviewer comments 3
* .onLoad() attempted to install from github rather than 
install the version on CRAN, prohibited in the policy.
  * The .onLoad function was removed.
  
## Reviewer comments 2
* Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_exploratory.html>.
Please correct before 2021-04-08 to safely retain your package on CRAN.
  * The problem was fixed.

## Reviewer comments 1
* Please reduce the length of the title to less than 65 characters.
  * The title has been changed.

* If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file...
  * References are added to the description.

* Please add `\value` to .Rd files regarding exported methods and explain
the functions results in the documentation...
  * `\value` has been added for each function.

## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

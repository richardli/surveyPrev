## Version 2.0.0 Submission

This is a major update (version 2.0.0) of surveyPrev. The previous CRAN release was 1.0.0. It adds a large set of standardized DHS indicators, child-mortality models, covariate support, and new mapping/plotting functions, and removes/renames several legacy functions (see NEWS.md).

## Test environments

* local: macOS (aarch64-apple-darwin), R 4.4.1
* win-builder: R-devel (x86_64-w64-mingw32), 2026-06-15
* win-builder: R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
   'unable to verify current time'
  This is an intermittent NOTE caused by the check host being unable to
  reach the remote time server. It is unrelated to the package.

* The Suggested package 'INLA' is not on a mainstream repository. It is
  available from the repository declared in 'Additional_repositories'
  (https://inla.r-inla-download.org/R/testing/). All examples that use INLA, or that require registration with DHS to
  download data, are wrapped in \dontrun{}.
  
## Reverse dependencies

This release renames/removes some previously exported objects, including the 'surveyPrevIndicators' dataset (now replaced by 'indicatorList'). We checked the one reverse dependency on CRAN, 'sae4health'. Its live code already uses 'indicatorList'. All surveyPrev functions used by 'sae4health' remain backward compatible.
---

## Version 1.0.0 Update
* The size of two directories leads to devtools::check() notes: data (3.0Mb) and doc (2.5Mb). The data directory contains necessary illustrative examples of complex survey data and maps. The doc directory contains static vignettes that provide more details in using the package. Dynamic vignettes are not possible because the computation of examples takes a long time and requires INLA installation. Since the package also concerns visualizing spatial smoothing models for fine geographic areas, the resulting vignettes are slightly larger in size as well. We have made the vignettes more concise and significantly reduced the size using the rmarkdown::html_vignette() output format. 
* Many examples in the package require either INLA or registration with DHS to download the data. These are the expected use cases for the package and are thus kept as is and wrapped with dontrun().

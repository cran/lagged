# lagged 0.2-1

* Bug-fix:  `nSeasons()` and `nSeasons<-()`are now exported.

* new functions `sl2acfbase()`, `acfbase2sl()` and `sl2vecacf()` for converting
  between (multivariate) autocovariances returned by `stats::acf()` and
  season-lag representation. (These functions were formerly in package pcts.)

* The title of the vignette and the keywords were wrong.

* added the pkgdown site to DESCRIPTION.


# lagged 0.2-0 (CRAN)

* New generics `nSeasons()` and `nSeasons<-()` (moved here from package sarima).

* New `slMatrix` function and class (moved from package pcts).

* Numerous changes to the documentation.


# lagged 0.1-2

* Lagged classes now have methods for "Maths" group generic.

* Lagged classes now have methods for "Summary" group generic.

* Lagged classes now have methods for "Ops" unary operators (binary Ops were
  already available).

* New vignette `Guide_lagged` (first draft).

* New function `maxLag<-()` with methods for Lagged objects.


# lagged 0.1-1 (CRAN)

* New function `dataWithLagNames()`, mainly for programming.


# lagged 0.1-0 (CRAN)

* First CRAN version (taken out of package sarima).

# lagged 0.3.2 (CRAN)

* Fixed 'LaTeX specials' note from CRAN.

* Minor documentation tweaks.


# lagged 0.3-1 (CRAN)

* made sure that `Lagged` checks early if the argument inherits from
  `"acf"`. This fixes errors in R-devel checks of lagged (R-devel from
  2021-12-03 or earlier), since now `is.vector` gives TRUE for objects from S3
  class `"acf"`, while previously it returned FALSE.

* removed travis settings.

* improved handling of `"[["` for `Lagged2d` objects.

* for `Lagged2d` objects, added "[" methods for character `i`.


# lagged 0.3-0 (CRAN)

* updated the vignette.


# lagged 0.2-2

* added the missing `show()` method for `Lagged2d` objects.

* added `[[` method for `slMatrix` objects.

* new `as()` method to convert `matrix` to `slMatrix`.

* the initialisation method for `FlexibleLagged` now matches argument `data`
  only if it is named, i.e. when explicitly supplied. This puts it in line with
  the default initialisation methods and, in particular, makes it easier to
  initialise subclasses of `FlexibleLagged`.

- set a prototype, `new("Lagged1d")`, for the data slot of `FlexibleLagged`. The
  default prototype was giving invalid objects in some cases,
  e.g. `new("FlexibleLagged")`.

- added `"maxLag<-"` method for `FlexibleLagged` objects.

- the "[[" and "[" methods for `FlexibleLagged` were not completely in line with
  the other Lagged classes (and the documentation) for index of length one.

- for "Lagged2d" objects indexing with two indices is now defined and works
  naturally in a matrix-like way: the first index, `i`, takes values in `1:d`,
  where `d` is the number of seasons and the second index, `j`, in
  `0:maxlag`. As for matrices, an empty index designates the whole range in the
  corresponding extent.


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

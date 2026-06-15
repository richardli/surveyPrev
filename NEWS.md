# surveyPrev - changes

Version 2.0.0
==========================

Indicators
+ Significantly expanded indicator support (now 183 indicators in the new
  `indicatorList` dataset), covering mortality, antenatal/postnatal and
  delivery care, family planning, child nutrition/vaccination, malaria,
  WASH, wealth, education, and FGM chapters.
+ Indicator functions renamed to standardized DHS recode codes,  
   with legacy exported functions removed. The `surveyPrevIndicators` dataset is replaced by `indicatorList`.
+ Added 2-year and 5-year "preceding survey" variants for many indicators,
  plus many country/survey-specific fixes.
+ `addNewIndicator()` to register a user-defined processing function.

Models
+ Child mortality support (U5MR, IMR, NMR) for direct, Fay-Herriot, and
  cluster-level models, using person-month formatting via `getBirths()`.
+ Variance adjustment for direct estimates with sparse data.
+ Area-level covariates via `getCovariate()`, unit-level covariates and
  nested models in `clusterModel()`, and Admin1 × urban/rural stratification.
+ Direct estimates and cluster models can return posterior samples.

Data download & spatial input
+ `getDHSdata()` gains a `SurveyType` argument (DHS/SPE) and handles
  surveys with multiple datasets.
+ geoBoundaries support as an alternative to GADM (`get_geoBoundaries()`,
  `addUpper()`).
+ `clusterInfo()` now flags invalid GPS, points outside boundaries from
  jittering, and Admin1/Admin2 mismatches (`wrong.points`); `poly.adm2`
  may be NULL.

Visualization
+ New plotting functions: interactive leaflet maps (`prevMap.web()`),
  static maps, exceedance-probability maps, rank plots, and interval
  plots (Admin 2 grouped by Admin 1, multi-model comparison).

Output & infrastructure
+ Standardized output columns and a unified `cv` definition
+ Reduced dependencies and CRAN-readiness fixes. 
+ Updated vignettes/examples.


Version 1.0.0
==========================
+ More flexible model fitting syntax.
+ New indicators included.
+ New plotting functions included. 
+ Two new vignettes. 


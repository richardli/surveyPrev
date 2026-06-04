#' Table of supported DHS indicators.
#'
#' A data frame listing the DHS indicators supported by the package, including
#' their IDs, descriptions, full definitions, associated DHS chapters, and the
#' recode files (IR, PR, KR, BR, HR, MR, AR, CR) required to compute them.
#'
#' @docType data
#'
#' @usage data(indicatorList)
#'
#' @format A data frame with 182 rows and 15 columns:
#' \describe{
#'   \item{Chap_abbrev}{Chapter acronym.}
#'   \item{ID}{DHS standard indicator ID.}
#'   \item{Description}{Short label of the indicator.}
#'   \item{Full_definition}{Full definition of the indicator.}
#'   \item{Topic}{Chapter topic the indicator belongs to.}
#'   \item{IR, PR, KR, BR, HR, MR, AR, CR}{Logical flags indicating which DHS
#'     recode files are required.}
#'   \item{Chapter}{DHS chapter number.}
#'   \item{Title}{DHS chapter title.}
#' }
#'
"indicatorList"

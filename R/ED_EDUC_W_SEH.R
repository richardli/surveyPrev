#'ED_EDUC_W_SEH
#'IRdata
#'Percentage of women with secondary or higher education
#'
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "FP_CUSA_W_MOD",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::fp_cruse_mod)
#' }
#' @export
#'
ED_EDUC_W_SEH <- function(Rdata){


  IRdata <- Rdata %>%
    mutate(wt = v005/1000000) %>%
    mutate(residence = v025) %>%
    mutate(region = v024) %>%
    mutate(wealth = v190) %>%
    mutate(education = v106)

  IRdata <- expss::apply_labels(
    IRdata,
    residence = c("urban" = 1, "rural" = 2),
    education = c("No education" = 0, "Primary" = 1, "Secondary" = 2, "More than secondary" = 3),
    wealth = c("poorest" = 1, "poorer" = 2, "middle" = 3, "richer" = 4, "richest" = 5)
  )

  IRdata$education=ifelse(IRdata$education %in% c(2,3), 1, 0)

  colnames(IRdata)[colnames(IRdata) == 'education'] <- "value"


  return(IRdata)
}

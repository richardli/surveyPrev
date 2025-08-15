#'FG_KFCC_W_HFC
#'IRdata
#'Percentage of women who have ever heard of female circumcision
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
FG_KFCC_W_HFC <- function(Rdata){

    # # weight variable
    IRdata <- Rdata %>%
      mutate(wt = v005/1000000)

    # # //Heard of female circumcision
    # IRdata <- IRdata %>%
    #   mutate(fg_heard =
    #            case_when(
    #              g100==1 | g101==1 ~ 1 ,
    #              g100==0 & g101==0 ~ 0 )) %>%
    #   set_value_labels(fg_heard = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(fg_heard = "Heard of female circumcision")
    #
    # QY: 04/03/2025, g101 all NA in HG2018, so make and& to or|
    # //Heard of female circumcision
    IRdata <- IRdata %>%
      mutate(FG_KFCC_W_HFC =
               case_when(
                 g100==1 | g101==1 ~ 1 ,
                 g100==0 | g101==0 ~ 0 )) %>%
      set_value_labels(FG_KFCC_W_HFC = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(FG_KFCC_W_HFC = "Heard of female circumcision")

    colnames(IRdata)[colnames(IRdata) == 'FG_KFCC_W_HFC'] <- "value"
    return(IRdata)


  }





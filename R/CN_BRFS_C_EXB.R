#'CN_BRFS_C_EXB
#'KRdata
#'Children exclusively breastfed
#'Prevalence of exclusive breastfeeding of children under six months of age
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_BRFS_C_EXB",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::CN_BRFS_C_EXB)
#' }
#' @export
#'
CN_BRFS_C_EXB<- function(Rdata){

  KRdata=Rdata
  # Calculate age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  KRiycf <- KRdata %>%
    subset(age < 24 & b9==0) %>% # children under 24 months living at home
    arrange(caseid, bidx) %>% # make sure the data is sorted
    subset(is.na(lag(caseid)) | caseid!=lag(caseid)) # select just the youngest

  KRiycf <- KRiycf %>%
    mutate(wt = v005/1000000)

  # *** Breastfeeding and complemenatry feeding ***
  #
  # //currently breastfed
  KRiycf <- KRiycf %>%
    mutate(nt_bf_curr =
             case_when(
               m4==95  ~ 1 ,
               m4 %in% c(93,94,98,99) ~ 0)) %>%
    set_value_labels(nt_bf_curr = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_bf_curr = "Currently breastfeeding - last-born under 2 years")

  # //breastfeeding status
  KRiycf <- KRiycf %>%
    mutate(water  = case_when(v409==1  ~ 1 , v409!=1 ~ 0)) %>%
    mutate(liquids= case_when(v409a==1 | v410==1 | v410a==1 | v412c==1 | v413==1 | v413a==1 | v413b==1 | v413c==1 | v413d==1  ~ 1 ,
                              v409a!=1 | v410!=1 | v410a!=1 | v412c!=1 | v413!=1 | v413a!=1 | v413b!=1 | v413c!=1 | v413d!=1 ~ 0)) %>%
    mutate(milk   = case_when(v411==1 | v411a==1 ~ 1 , v411!=1 | v411a!=1 ~ 0)) %>%
    mutate(solids = case_when(v414a==1 | v414b==1 | v414c==1 | v414d==1 | v414e==1 | v414f==1 | v414g==1 | v414h==1 | v414i==1 |
                                v414j==1 | v414k==1 | v414l==1 | v414m==1 | v414n==1 | v414o==1 | v414p==1 | v414q==1 | v414r==1 |
                                v414s==1 | v414t==1 | v414u==1 | v414v==1 | v414w==1 | v412a==1 | v412b==1 | m39a==1 ~ 1 ,
                              v414a!=1 | v414b!=1 | v414c!=1 | v414d!=1 | v414e!=1 | v414f!=1 | v414g!=1 | v414h!=1 | v414i!=1 |
                                v414j!=1 | v414k!=1 | v414l!=1 | v414m!=1 | v414n!=1 | v414o!=1 | v414p!=1 | v414q!=1 | v414r!=1 |
                                v414s!=1 | v414t!=1 | v414u!=1 | v414v!=1 | v414w!=1 | v412a!=1 | v412b!=1 | m39a!=1~ 0) ) %>%
    mutate(nt_bf_status = case_when(nt_bf_curr==0 ~ 0, solids==1 ~ 5, milk==1 ~ 4, liquids==1 ~3, water==1 ~2, TRUE~1 )) %>%
    set_value_labels(nt_bf_status = c("not bf"=0, "exclusively bf"=1, "bf & plain water"=2, "bf & non-milk liquids"=3, "bf & other milk"=4, "bf & complemenatry foods"=5 )) %>%
    set_variable_labels(nt_bf_status = "Breastfeeding status for last-born child under 2 years")
  # //exclusively breastfed
  KRiycf <- KRiycf %>%
    mutate(nt_ebf =
             case_when(
               age<6 & nt_bf_status==1  ~ 1 ,
               age<6 & nt_bf_status!=1 ~ 0)) %>%
    set_value_labels(nt_ebf = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_ebf = "Exclusively breastfed - last-born under 6 months")
  colnames(KRiycf)[colnames(KRiycf) == 'nt_ebf'] <- "value"
  return(KRiycf)
}

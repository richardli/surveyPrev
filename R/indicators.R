#' Internal function to compute "Any anemia - women"
#'
#' @param RData list returned from getDHSdata when indicator is specified as NULL
#'
#'

AN_ANEM_W_ANY <- function(RData){
	 IRdata <- RData$IRdata %>%
      mutate(nt_wm_any_anem =
               case_when(
                 v042==1 & v457<4 ~ 1 ,
                 v042==1 &  v455==0 ~ 0)) %>%
      set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
      set_variable_labels(nt_wm_any_anem = "Any anemia - women")

      colnames(IRdata)[colnames(IRdata) == 'nt_wm_any_anem'] <- "value"
    return(IRdata)
}





n_un_fam_plan<- function(RData){
  # IRdata <- Rdata %>%
  #   mutate(wt = v005/1000000)
  #
  IRdata <- RData$IRdata %>%
    mutate(n_un_fam_plan =
             case_when(
               v626 == 1 ~ 1,
               v626 == 2 ~ 1,
               v626 == 3 ~ 0,
               v626 == 4 ~ 0,
               v626 %in% c(5, 6, 7, 8, 9) ~ NA)) %>%
    set_value_labels(n_un_fam_plan = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(n_un_fam_plan = "Unmet need for family planning")

  colnames(IRdata)[colnames(IRdata) == 'n_un_fam_plan'] <- "value"
  return(IRdata)
}

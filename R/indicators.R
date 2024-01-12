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
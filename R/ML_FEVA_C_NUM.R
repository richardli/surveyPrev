#' Number of children under 5 with fever for whom advice of treatment was sought
#'
#' Number of children under age five with fever in the two weeks preceding the survey for whom advice of treatment was sought
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ML/ML_FEVA_C_NUM.R; validated on ZM_2018 (published 1141, observed 1141.1489, abs_error 0.1489).
#' @export
ML_FEVA_C_NUM <- function(KRdata) {
  # Number of children under 5 with fever for whom advice/treatment was sought.
  # Denominator: living children with fever in the 2 weeks preceding the survey
  #   (b5==1 & h22==1). Numerator: advice/treatment sought from any source
  #   EXCEPT traditional practitioner (h32t) — per DHS github ML_FEVER ml_fev_care.
  # @source LLM-authored | seed=ML_FEVA_C_NUM | iter=7 | fix=exclude h32t
  #   (traditional practitioner) from the advice-sought source set; validated
  #   as a weighted count on ZM_2018.
  df <- KRdata[KRdata$b5 == 1 & KRdata$h22 == 1 & !is.na(KRdata$h22), , drop = FALSE]
  sources <- setdiff(grep("^h32[a-x]$", names(df), value = TRUE), "h32t")
  mat <- sapply(sources, function(v) as.numeric(haven::zap_labels(df[[v]])) == 1)
  df$value <- as.numeric(rowSums(mat, na.rm = TRUE) > 0)
  df <- df[, c("v001", "v002", "v005", "v022", "v023", "v024", "v025", "value")]
  return(as.data.frame(df))
}

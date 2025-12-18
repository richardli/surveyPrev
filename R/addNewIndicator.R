##' Create new row to add in indicatorList.
##'
##'
#' @param new_id The ID of the new indicator (e.g., "FE_FRTR_W_A10").
#' @param dictionary The DHS indicator dictionary data frame, e.g.sae4health::DHS_ind_dictionary
#' @param chapters The DHS chapters reference data frame.
#' @param ... Boolean flags for IR, PR, KR, BR, HR, MR, AR, CR.
#' @importFrom stringr str_split
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' newrow=addNewIndicator(new_id="RH_DELP_C_HOM", dictionary= sae4health::DHS_ind_dictionary,
#' IR=FALSE, PR=FALSE, KR=FALSE, BR=TRUE,
#' HR=FALSE, MR=FALSE, AR=FALSE, CR=FALSE )
#' newrow <- newrow[, names(indicatorList)]
#'indicatorList <- rbind(indicatorList, newrow)
#' }
#'
#'
#' @export
#' @return The a row to rbind into indicator_df
addNewIndicator <- function(new_id, dictionary,
                              IR=FALSE, PR=FALSE, KR=FALSE, BR=FALSE,
                              HR=FALSE, MR=FALSE, AR=FALSE, CR=FALSE) {

  # --- Step A: Extract information based on the new ID ---

  # Get the chapter acronym from the ID (the part before the first '_')
  chap_acronym <- str_split(new_id, "_")[[1]][1]
  dhs_chapters <- data.frame(
    Chapter = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    Title = c("Housing Characteristics And Household Population",
              "Characteristics Of Respondents",
              "Marriage And Sexual Activity",
              "Fertility",
              "Fertility Preferences",
              "Family Planning",
              "Infant And Child Mortality",
              "Maternal Health",
              "Child Health",
              "Nutrition Of Children And Adults",
              "Malaria",
              "HIV/AIDS-Related Knowledge, Attitudes, And Behaviour",
              "HIV Prevalence",
              "Women's Empowerment",
              "Adult And Maternal Mortality",
              "Domestic Violence",
              "Female Genital Cutting",
              "Fistula"),
    Acronym = c("PH", "RC", "MS", "FE", "FF", "FP", "CM", "RH", "CH", "NT", "ML", "HK", "HV", "WE", "AM", "DV", "FG", "FS")
  )




  chapter_info <- dhs_chapters[dhs_chapters$Acronym == chap_acronym, ]

  if (nrow(chapter_info) == 0) {
    stop("Chapter acronym '", chap_acronym, "' not found in dhs_chapters.")
  }

  # Look up indicator details from the DHS dictionary
  indicator_details <- dictionary[dictionary$`DHS Standard Indicator ID` == new_id, ]

  if (nrow(indicator_details) == 0) {
    stop("ID '", new_id, "' not found in the DHS dictionary.")
  }

  # --- Step B: Assemble the new row ---

  new_row <- data.frame(
    Chap_abbrev = chapter_info$Acronym,
    ID = new_id,
    Description = indicator_details$Label,
    Full_definition = indicator_details$`Full Definition`,
    Topic = paste("Chapter", sprintf("%02d", chapter_info$Chapter), "-", chapter_info$Title),
    IR = IR,
    PR = PR,
    KR = KR,
    BR = BR,
    HR = HR,
    MR = MR,
    AR = AR,
    CR = CR,
    Chapter = chapter_info$Chapter,
    Title = chapter_info$Title,
    stringsAsFactors = FALSE
  )


  # Ensure column order matches before binding
  # new_row <- new_row[, names(indicator_df)]
  # updated_df <- rbind(indicator_df, new_row)

  return(new_row)
}

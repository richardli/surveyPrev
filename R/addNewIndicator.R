##' Create new row to add in indicatorList.
##'
##'
#' @param new_id The ID of the new indicator (e.g., "FE_FRTR_W_A10").
#' @param dictionary The DHS indicator dictionary data frame, e.g.sae4health::DHS_ind_dictionary
#' @param IR Logical flag indicating whether the IR recode file is required.
#' @param PR Logical flag indicating whether the PR recode file is required.
#' @param KR Logical flag indicating whether the KR recode file is required.
#' @param BR Logical flag indicating whether the BR recode file is required.
#' @param HR Logical flag indicating whether the HR recode file is required.
#' @param MR Logical flag indicating whether the MR recode file is required.
#' @param AR Logical flag indicating whether the AR recode file is required.
#' @param CR Logical flag indicating whether the CR recode file is required.
#' @param chapter_map Optional data.frame giving the report chapter per ID,
#'   e.g. \code{read.csv("sideproject/results/passing_current/_chapter_map.csv")}
#'   from the DHS-indicator-factory repo. Must have columns \code{dhs_id} and
#'   \code{chap_abbrev}; an optional \code{recode} column ("IR", "PR", ...)
#'   fills the recode flag when all eight flags are left FALSE. When
#'   \code{new_id} is found in the map, its \code{chap_abbrev} is used and the
#'   built-in prefix/topic crosswalk is bypassed.
#' @details
#' The chapter comes from \code{chapter_map} when it holds \code{new_id};
#' otherwise the ID prefix (the token before the first underscore) must itself
#' be a DHS report-chapter acronym (e.g. \code{FP}, \code{RH}, \code{CH}).
#' StatCompiler chapter codes that are not report acronyms (\code{AH},
#' \code{ED}, \code{CO}, \code{HA}, \code{HC}, \code{WS}, \code{MA},
#' \code{CN}, \code{FI}, ...) therefore require a \code{chapter_map} row.
#' Chapter numbers and titles follow the package's existing
#' \code{indicatorList} (DHS-7 report numbering: WASH sits in Chapter 02,
#' Fistula is Chapter 19).
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
                              HR=FALSE, MR=FALSE, AR=FALSE, CR=FALSE,
                              chapter_map = NULL) {

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




  # Chapter map (e.g. DHS-indicator-factory
  # sideproject/results/passing_current/_chapter_map.csv) supplies the report
  # chapter; without a hit, the ID prefix must be a report-chapter acronym.
  map_hit <- NULL
  if (!is.null(chapter_map)) {
    if (!all(c("dhs_id", "chap_abbrev") %in% names(chapter_map))) {
      stop("`chapter_map` must have columns `dhs_id` and `chap_abbrev`.")
    }
    map_hit <- chapter_map[chapter_map$dhs_id == new_id, , drop = FALSE]
    if (nrow(map_hit) == 0) map_hit <- NULL
  }
  if (!is.null(map_hit)) {
    chap_acronym <- as.character(map_hit$chap_abbrev[1])
  }

  # Recode flag from the map when the caller left all flags FALSE.
  if (!is.null(map_hit) && "recode" %in% names(map_hit) &&
      !any(c(IR, PR, KR, BR, HR, MR, AR, CR))) {
    rc <- toupper(as.character(map_hit$recode[1]))
    if (rc %in% c("IR", "PR", "KR", "BR", "HR", "MR", "AR", "CR")) {
      assign(rc, TRUE)
    } else {
      warning("chapter_map recode '", rc, "' for ", new_id,
              " not recognised; all recode flags left FALSE.")
    }
  }

  chapter_info <- dhs_chapters[dhs_chapters$Acronym == chap_acronym, ]

  if (nrow(chapter_info) == 0) {
    stop("Chapter acronym '", chap_acronym, "' is not a DHS report-chapter ",
         "acronym. Supply `chapter_map` with a row for ", new_id, ".")
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

#'RH_DELA_C_SKF
#'BRdata
#'Assistance during delivery from a skilled provider
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
#'                                  indicator = "RH_DELA_C_SKF",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::RH_DELA_C_SKF)
#' }
#' @export
#'
RH_DELA_C_SKF <- function(Rdata){

  BRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # this is fiver year
  BRdata <- BRdata %>%
    mutate(period = 60)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(BRdata))))
    BRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(BRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    BRdata <- BRdata %>%
      mutate(age = b19)
  } else {
    BRdata <- BRdata %>%
      mutate(age = v008 - b3)
  }

  # ** NEW STEP: Explicitly filter the data to keep only recent births **
  # This directly changes the number of rows (the sample size).
  BRdata <- BRdata %>%
    filter(age < period)


  BRdata <- BRdata %>%
    mutate(rh_del_pv =
             case_when(
               m3a == 1   ~ 1 ,
               m3b == 1 ~ 2,
               m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
               m3g == 1 ~ 4 ,
               m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
               m3n ==1 ~ 6,
               m3a ==8 | m3a==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pv = c(99))) %>%
    set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

  # //Skilled provider during delivery
  # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
  BRdata <- BRdata %>%
    mutate(rh_del_pvskill =
             case_when(
               rh_del_pv %in% c(1,2)   ~ 1 ,
               rh_del_pv %in% c(3,4,5) ~ 2,
               rh_del_pv ==6 ~ 3 ,
               rh_del_pv==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
    set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")

  if (BRdata$v000[1] %in% c("NG7","NG6")) {
    # //Skilled provider during delivery -- ****SPECIFIC FOR NIGERIA*******
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 #m3c==1 ~ 1,
                 rh_del_pv %in% c(1,2) | m3c==1   ~ 1 , # ADD AUXILIARY MIDWIFE HERE FOR NIGERIA (M3C==1)
                 rh_del_pv %in% c(3, 4, 5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery in Nigeria")
  }

  if (BRdata$v000[1] %in% c("BF8","BF6","TZ8","ET7","ET8","CD6","SN7","SN8","MZ8","RW6","RW7","SL6","SL7","ZM7","ZM8","MZ6")) {

    ## "Country specific health professional"=3 AS Skilled provider FOR "BF8","BF6","TZ8" ,"ET7","ET8","CD6","SN8","MZ8"


    # BF2021 BF8
    # Prenatal care provided by a skilled provider
    # Pregnancy-related care received from skilled providers, such as
    # physicians, health counselors, midwives, registered midwives, nurses,
    # health officers, certified birth attendants and auxiliary birth attendants,
    # mobile health workers, and community health workers
    # Sample: Women aged 15–49 who had a live birth or a stillbirth
    # in the 2 years preceding the survey.
    # Soins prénatals dispensés par un prestataire qualifié
    # Soins relatifs à la grossesse reçus de prestataires qualifiés, comme les
    # médecins, conseillers de santé, sages-femmes, maïeuticiens d’état, infirmiers,
    # attachés de santé, accoucheuses brevetées et les accoucheuses auxiliaires,
    # agent itinérant de santé et agent de santé à base communautaire
    # Échantillon : Femmes de 15–49 ans ayant eu une naissance vivante ou un
    # mort-né au cours des 2 années précédant l’enquête.


    # vars <- grep("^m3[a-n]$", names(BRdata), value = TRUE)
    #
    # # pull labels
    # labels <- sapply(vars, function(v) attr(BRdata[[v]], "label"))
    #
    # # combine into a data.frame
    # labelf=data.frame(variable = vars, label = labels, stringsAsFactors = FALSE)


    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 #m3c==1 ~ 1,
                 rh_del_pv %in% c(1,2,3)  ~ 1 ,  # "Country specific health professional"=3 AS Skilled provider
                 rh_del_pv %in% c( 4, 5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery in Nigeria")


  }

  if (BRdata$v000[1] %in% c("TZ7")) {

    # TZ7 has different column definitions
    # Skilled assistance during delivery
    # Births delivered with the assistance of doctors, assistant medical officers,
    # clinical officers/assistant clinical officers, nurse/midwives, and MCH aides
    # Sample: All live births in the 5 years before the survey

    # variable                                             label
    # m3a      m3a                            assistance: doctor/amo
    # m3b      m3b                      assistance: clinical officer
    # m3c      m3c            assistance: assistant clinical officer
    # m3d      m3d           na - assistance: cs health professional
    # m3e      m3e           na - assistance: cs health professional
    # m3f      m3f           na - assistance: cs health professional
    # m3g      m3g                        assistance: nurse/ midwife
    # m3h      m3h                         assistance: assist. nurse
    # m3i      m3i assistance: maternal and child health (mch) aides
    # m3j      m3j         assistance: community health worker (chw)
    # m3k      m3k                                 assistance: other
    # m3l      m3l    assistance: traditional birth attendants (tba)
    # m3m      m3m                       assistance: relative/friend
    # m3n      m3n                                assistance: no one

    # TZ8
    # > labelf
    # variable                                   label
    # m3a      m3a                  assistance: doctor/amo
    # m3b      m3b            assistance: clinical officer
    # m3c      m3c  assistance: assistant clinical officer
    # m3d      m3d               assistance: nurse/midwife
    # m3e      m3e             assistance: assistant nurse
    # m3f      m3f                    assistance: mch aide
    # m3g      m3g assistance: traditional birth attendant
    # m3h      m3h     assistance: community health worker
    # m3i      m3i           assistance: relative/neighbor
    # m3j      m3j        na - assistance: cs other person
    # m3k      m3k                       assistance: other
    # m3l      m3l               na - assistance: cs other
    # m3m      m3m               na - assistance: cs other
    # m3n      m3n                      assistance: no one

    # vars <- grep("^m3[a-n]$", names(BRdata), value = TRUE)
    #
    # # pull labels
    # labels <- sapply(vars, function(v) attr(BRdata[[v]], "label"))
    #
    # # combine into a data.frame
    # labelf=data.frame(variable = vars, label = labels, stringsAsFactors = FALSE)


    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 ~ 2,
                 m3c == 1 | m3g == 1 | m3h == 1 | m3i == 1~ 3 ,
                 m3l == 1 ~ 4 ,
                 m3j == 1 | m3f == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")



    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 #m3c==1 ~ 1,
                 rh_del_pv %in% c(1,2,3)  ~ 1 ,  # "Country specific health professional"=3 AS Skilled provider FOR BF
                 rh_del_pv %in% c( 4, 5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery in Nigeria")


  }



  if (BRdata$v000[1] %in% c("MZ6")) {
    #
    # labelf
    # variable                                         label
    # m3a      m3a                            assistance: doctor
    # m3b      m3b           assistance: nurse/medical assistant
    # m3c      m3c                           assistance: midwife
    # m3d      m3d       na - assistance: cs health professional
    # m3e      m3e       na - assistance: cs health professional
    # m3f      m3f       na - assistance: cs health professional
    # m3g      m3g                assistance: traditional healer
    # m3h      m3h           assistance: community health worker
    # m3i      m3i assistance: community health mother and child
    # m3j      m3j                         assistance: relatives
    # m3k      m3k             assistance: friends and neighbors
    # m3l      m3l                     na - assistance: cs other
    # m3m      m3m                             assistance: other
    # m3n      m3n                            assistance: no one
    # >
    #

    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1 ~ 1 ,
                 m3b == 1 ~ 2,
                 m3c == 1 ~ 3 ,
                 m3g == 1 ~ 4 ,
                 m3d == 1  | m3h == 1 | m3e == 1 | m3f == 1  | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2,3)  ~ 1 ,  # "Country specific health professional"=3 AS Skilled provider
                 rh_del_pv %in% c( 4, 5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")


  }



  if (BRdata$v000[1] %in% c("ML7")) {
    #ML7
    # labelf
    # variable                                           label
    # m3a      m3a                              assistance: doctor
    # m3b      m3b                       assistance: nurse/midwife
    # m3c      m3c                              assistance: matron
    # m3d      m3d assistance: trained traditional birth attendant
    # m3e      m3e         na - assistance: cs health professional
    # m3f      m3f         na - assistance: cs health professional
    # m3g      m3g         assistance: traditional birth attendant
    # m3h      m3h             assistance: community health worker
    # m3i      m3i                    assistance: relative/ friend
    # m3j      m3j                na - assistance: cs other person
    # m3k      m3k                               assistance: other
    # m3l      m3l                       na - assistance: cs other
    # m3m      m3m                       na - assistance: cs other
    # m3n      m3n                              assistance: no one

    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 ~ 2,
                 m3c == 1 ~ 3 ,
                 m3g == 1 ~ 4 ,
                 m3d == 1  | m3h == 1 | m3e == 1 | m3f == 1  | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2,3)  ~ 1 ,  # "Country specific health professional"=3 AS Skilled provider
                 rh_del_pv %in% c( 4, 5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")


  }



  BRdata$rh_del_pvskill= ifelse( BRdata$rh_del_pvskill == 1, 1, 0)
  colnames(BRdata)[colnames(BRdata) == 'rh_del_pvskill'] <- "value"
  return(BRdata)
}

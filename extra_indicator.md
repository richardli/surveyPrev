Indicator IDs not included in the DHS list (for version control, e.g., two-year vs. five-year): CM_ECMR_C_NNF, HC_WIXQ_P_12Q, RH_ANCN_W_N4F, RH_DELA_C_SKF, RH_DELP_C_DHT, RH_DELP_C_HOT, RH_DELP_C_PRT, RH_DELP_C_PUT, CM_ECMR_C_U5F, CM_ECMR_C_IMF

Checked against the live DHS Program indicator registry
(https://api.dhsprogram.com/rest/dhs/indicators, snapshot in DHS-indicator-factory
data/dhs-reference/indicators_live.csv). Earlier versions of this line used
sae4health::DHS_ind_dictionary, a 2024 snapshot that lacks newer DHS-8 IDs.

## Change log — indicatorList

### 2026-09-10 — 56 existing package functions added to indicatorList (302 -> 358 rows)

Functions already in R/ that had no indicatorList row, so `getDHSindicator(indicator=)`
could not dispatch them. 50 were copied from DHS-indicator-factory on 2026-07-09
(commit 38544ce) but skipped by that migration's chapter-prefix whitelist; 6 were older
hand-added functions. Chapter assigned via the prefix->report-chapter crosswalk
(later replaced by the factory's passing_current/_chapter_map.csv). No function code changed.

- **Chapter 02 - Housing Characteristics And Household Population** (20): ED_EDAT_B_CSC, ED_EDAT_B_HGH, ED_EDAT_B_NED, ED_EDAT_M_PRI, ED_EDAT_M_SPR, ED_EDAT_W_CSC, ED_EDAT_W_SPR, HC_TRNS_H_CAR, HC_TRNS_H_SCT, WS_HNDW_H_BAS, WS_HNDW_H_OBS, WS_SRCE_H_BAS, WS_SRCE_H_IMP, WS_SRCE_H_LTD, WS_SRCE_H_USG, WS_TLET_H_BAS, WS_TLET_P_IMP, WS_WTRT_H_BLC, WS_WTRT_H_BOL, WS_WTRT_H_STN
- **Chapter 03 - Characteristics Of Respondents** (13): AH_HINS_W_ANY, CO_INUS_M_EVU, CO_INUS_M_U12, ED_EDUC_W_CPR, ED_EDUC_W_CSC, ED_EDUC_W_HGH, ED_EDUC_W_NED, ED_EDUC_W_PRI, ED_EDUC_W_SPR, ED_EDUC_W_SSC, ED_LITR_M_SCH, ED_LITR_W_RDW, ED_LITR_W_SCH
- **Chapter 04 - Marriage And Sexual Activity** (5): MA_MBAG_W_B15, MA_MBAG_W_B18, MA_MSTA_W_UNI, MA_MSTY_W_MAR, MA_MSTY_W_UNI
- **Chapter 08 - Infant And Child Mortality** (2): CM_ECMR_C_IMF, CM_ECMR_C_IMR
- **Chapter 11 - Nutrition Of Children And Adults** (5): CN_IYCF_C_4FA, CN_IYCF_C_BTB, CN_IYCF_C_MNA, CN_IYCF_C_MNB, CN_MIAC_C_IRF
- **Chapter 13 - HIV/AIDS-Related Knowledge, Attitudes, And Behaviour** (3): HA_CKNA_W_CKA, HA_KAID_M_HRD, HA_KHVP_M_CS1
- **Chapter 15 - Women's Empowerment** (6): CO_MOBB_M_BNK, CO_MOBB_M_MBF, CO_MOBB_M_MOB, CO_MOBB_M_SPT, CO_MOBB_W_DFT, CO_MOBB_W_SPT
- **Chapter 19 - Fistula** (2): FI_EXFI_W_EEX, FI_EXFI_W_HRD

### 2026-09-12 — 76 new functions copied from DHS-indicator-factory and listed (358 -> 434 rows)

Source: DHS-indicator-factory sideproject/results/passing_current/ (249 validated
functions; the other 173 were already in the package). Each file was copied into R/
with a generated roxygen header (label/definition from the StatCompiler export,
recode and chapter from _chapter_map.csv, provenance in @source), exported, and
registered with addNewIndicator(chapter_map=). 40 validated on TZ_2022, 36 on ZM_2018.

- **Chapter 02 - Housing Characteristics And Household Population** (12): ED_EDAT_B_CPR, ED_EDAT_B_PRI, ED_EDAT_B_SEC, ED_EDAT_B_SPR, ED_EDAT_M_CPR, ED_EDAT_M_CSC, ED_EDAT_M_NED, ED_EDAT_M_SEC, ED_EDAT_W_CPR, ED_EDAT_W_NED, ED_EDAT_W_PRI, ED_EDAT_W_SEC
- **Chapter 03 - Characteristics Of Respondents** (29): AH_BTHP_M_ALW, AH_BTHP_M_EMG, AH_BTHP_M_IMG, AH_BTHP_W_ALW, AH_BTHP_W_EMG, AH_BTHP_W_IMG, AH_HINS_M_ANY, AH_MIGR_M_DIS, AH_MIGR_M_EDU, AH_MIGR_M_EMP, AH_MIGR_M_FAM, AH_MIGR_M_MAR, AH_MIGR_W_EDU, AH_MIGR_W_EMP, AH_MIGR_W_FAM, AH_MIGR_W_MAR, AH_MIGT_M_RTR, AH_MIGT_M_RTU, AH_MIGT_M_UTR, AH_MIGT_M_UTU, AH_MIGT_W_RTR, AH_MIGT_W_RTU, AH_MIGT_W_UTR, AH_MIGT_W_UTU, CO_INUS_M_WEK, CO_INUS_W_WEK, ED_LITR_M_LIT, ED_MDIA_M_RDO, ED_MDIA_M_TLV
- **Chapter 07 - Family Planning** (1): FP_NDYM_W_TDT
- **Chapter 09 - Maternal Health** (19): RH_DELA_C_DOC, RH_DELA_C_NRS, RH_DELA_C_OHW, RH_DELP_C_PNG, RH_DELP_C_PNN, RH_DFHC_W_LTH, RH_DFHC_W_MOT, RH_DFHC_W_MTT, RH_DFHC_W_NMO, RH_DFHC_W_OTH, RH_DFHC_W_SXO, RH_DFHC_W_THF, RH_ICSP_W_B99, RH_ICSP_W_M18, RH_PCCT_C_H13, RH_PCCT_C_NON, RH_PCMP_W_NRS, RH_PCMT_W_23H, RH_PCMT_W_L4H
- **Chapter 10 - Child Health** (2): CH_DISA_C_NUM, CH_VACS_C_IP1
- **Chapter 12 - Malaria** (2): ML_CMLT_C_RDT, ML_FEVA_C_NUM
- **Chapter 13 - HIV/AIDS-Related Knowledge, Attitudes, And Behaviour** (1): HA_KHVP_W_CS1
- **Chapter 15 - Women's Empowerment** (4): CO_MOBB_W_DWM, CO_MOBB_W_OSP, WE_AWBT_M_AGR, WE_DMAK_M_OHC
- **Chapter 17 - Domestic Violence** (5): DV_FMVL_W_POS, DV_PCPV_W_CHP, DV_PCSV_W_EMP, DV_PCSV_W_RLG, DV_STPV_W_HLP
- **Chapter 18 - Female Genital Cutting** (1): FG_KFCC_M_HFC

Pending: 13 functions whose factory copy was revised after the July migration
(CH_VACS_C_PN3, CO_MOBB_M_MOB, CO_MOBB_W_MOB, ED_EDAT_B_CSC, ED_EDAT_B_HGH,
ED_EDAT_B_NED, ED_EDAT_M_PRI, ED_EDAT_M_SPR, ED_EDAT_W_CSC, ED_EDAT_W_SPR,
ED_MDIA_W_RDO, ED_MDIA_W_TLV, RH_ANCN_W_NON) still hold the July code in R/;
the seven ED_EDAT ones switch recode to PR when refreshed.

library(magrittr)


get_clean_dataset <- function(path="data/credit_scoring.xls", sheet = "Data") {
    
    #' READ RAW DATASET
    #' 
    raw_dataset <- readxl::read_excel(path = path, sheet = sheet, skip = 1) %>%
        dplyr::select(-ID)
    
    #' CLEANING:
    #' 
    #' - EDUCATION      : Undocumented labels 0, 5, 6 could be safely categorized as OTHERS
    #' - MARRIAGE       : undocumented label 0 can be safely categorized as OTHERS
    #' - PAY_N VARIABLES: Undocumented labels -2, 0. Seems reasonable to adjust to 0
    #'   i.e. -2, -1, and 0 should indicate pay duty (no delay).
    #'     
    credit_dataset <- raw_dataset %>% 
        
        dplyr::mutate(
            
            SEX = dplyr::recode(
                raw_dataset$SEX, 
                `1` = "MALE", `2` = "FEMALE"
            ),
            
            EDUCATION = dplyr::recode(
                raw_dataset$EDUCATION, 
                `1` = "GRAD_SCHO", `2` = "UNIV", `3` = "HIGH_SCHO", 
                `4` = "OTHERS", `0` = "OTHERS", `5` = "OTHERS", `6` = "OTHERS"
            ),
            
            MARRIAGE = dplyr::recode(
                raw_dataset$MARRIAGE, 
                `1` = "MARRIED", `2` = "SINGLE", `3` = "OTHERS", `0` = "OTHERS"
            ),
            
            PAY_0 = dplyr::case_when(
                raw_dataset$PAY_0 == -2 ~ 0,
                raw_dataset$PAY_0 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_0
            ),
            
            PAY_2 = dplyr::case_when(
                raw_dataset$PAY_2 == -2 ~ 0,
                raw_dataset$PAY_2 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_2
            ),
            
            PAY_3 = dplyr::case_when(
                raw_dataset$PAY_3 == -2 ~ 0,
                raw_dataset$PAY_3 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_3
            ),
            
            PAY_4 = dplyr::case_when(
                raw_dataset$PAY_4 == -2 ~ 0,
                raw_dataset$PAY_4 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_4
            ),
            
            PAY_5 = dplyr::case_when(
                raw_dataset$PAY_5 == -2 ~ 0,
                raw_dataset$PAY_5 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_5
            ),
            
            PAY_6 = dplyr::case_when(
                raw_dataset$PAY_6 == -2 ~ 0,
                raw_dataset$PAY_6 == -1 ~ 0,
                TRUE ~ raw_dataset$PAY_6
            ),
        ) %>%
        dplyr::rename("DEFAULT" = `default payment next month`)
    
    # Encode the Dependent Variable as a Factor
    credit_dataset %<>% dplyr::mutate(
        DEFAULT = as.factor(DEFAULT)
    )
    
    return(credit_dataset)
    
}







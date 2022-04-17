loadFib<- function(){
  fib<- read_sheet("https://docs.google.com/spreadsheets/d/12XVNy7HExfsiNtI3S0HOjzMteEaTpPhNC8p-1NKqF7A/edit#gid=0")
  fibCleaned<- fib %>% 
    select(sites, date_sample, test, correction_yel, correction_flo) %>%
    mutate(correction_yel = as.character(unlist(correction_yel)),
           correction_flo = as.character(unlist(correction_flo))) %>%
    mutate(correction_yel = gsub("<","",correction_yel),
           correction_flo = gsub("<", "", correction_flo)) %>%
    mutate(correction_yel = ifelse(correction_yel == "NA", 0, as.numeric(correction_yel)),
           correction_flo = ifelse(correction_flo == "NA", 0, as.numeric(correction_flo)))
  tc<- fibCleaned %>%
    filter(test == "TC") %>%
    rename(correction_flo_tc = correction_flo,
           correction_yel_tc = correction_yel)
  
  tc_esbl<- fibCleaned %>% 
    filter(test == "TC_ESBL") %>%
    rename(correction_flo_tc_esbl = correction_flo,
           correction_yel_tc_esbl = correction_yel)
  percent<- tc %>% 
    left_join(tc_esbl,  c("sites", "date_sample")) %>%
    mutate(percent_ecoli_resistant = correction_flo_tc_esbl/correction_flo_tc) %>% 
    mutate(percent_ecoli_resistant = ifelse(is.nan(percent_ecoli_resistant), 0, percent_ecoli_resistant)) %>%
    select(sites, date_sample, percent_ecoli_resistant, correction_flo_tc) %>%
    mutate(cat_ecoli_resistant = case_when(percent_ecoli_resistant == 0 ~ "0",
                                       percent_ecoli_resistant > 0 & percent_ecoli_resistant < 0.2 ~ "0<Per<20",
                                       percent_ecoli_resistant > 0.2 ~ ">20"))
  percent
}


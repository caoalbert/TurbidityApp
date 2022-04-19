loadArg<- function(){
  arg<- read_sheet("https://docs.google.com/spreadsheets/d/1_o6ucjGFvsUk3opf7Y_yXs-xZLWglIUB2xdoZ3aPPig/edit#gid=0")
  arg<- arg %>% 
    mutate(num_colonies = as.character(unlist(num_colonies))) %>% 
    mutate(num_colonies = gsub(">", "", num_colonies)) %>%
    filter(num_colonies != "INPROG" | num_colonies != "missing") %>%
    mutate(num_colonies = as.numeric((num_colonies))) %>%
    group_by(date_sample, sites, presence_antibiotics) %>%
    summarise(total_volume = sum(sample_vol_ml),
              total_colonies = sum(num_colonies)) %>%
    mutate(colonies_per_100ml = total_colonies/total_volume*100)
  
  with_ab<- arg %>%
    filter(presence_antibiotics == "T") %>%
    rename(with_ab_conc = colonies_per_100ml)
  
  without_ab<- arg %>%
    filter(presence_antibiotics == "F") %>%
    rename(without_ab_conc = colonies_per_100ml)
  
  arg_all<- with_ab %>% 
    left_join(without_ab, c("sites", "date_sample")) %>%
    mutate(percent_resistant = with_ab_conc/(without_ab_conc+without_ab_conc)) %>%
    select(sites, date_sample, percent_resistant, without_ab_conc) %>%
    mutate(cat_antibiotics = case_when(percent_resistant == 0 ~ "0",
                                       percent_resistant > 0 & percent_resistant < 0.2 ~ "0<Per<20",
                                       percent_resistant > 0.2 ~ ">20"))
  arg_all
}

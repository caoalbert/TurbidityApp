loadTurbidity<- function(){
  hydro<- read_sheet("https://docs.google.com/spreadsheets/d/18TUgYYC6s6ZbgMIdV4XYZ72J_nrS0BfzQUxzfqFy2YY/edit#gid=0")
  tss<- read_sheet("https://docs.google.com/spreadsheets/d/1-FOOyM14op9ojekoXiJ7Lx3qxvw4_lSBugtlzLVjdjU/edit#gid=0")
  hydro_list<- lapply(hydro, unlist)
  hydro<- cbind(hydro_list[1:3],
                as.data.frame(lapply(hydro_list[4:length(hydro_list)], as.numeric)))
  tss<- tss %>%
    mutate(tss_g_l = as.numeric(unlist(tss_g_l))) %>%
    group_by(date_sample, sites)%>%
    summarise(tss = mean(tss_g_l, na.rm = T))
  hydro %>%
    left_join(tss, c("date_sample", "sites")) %>%
    select(-sentinel_acolite_turbidity_sd_fnu, -landsat_acolite_turbidity_sd_fnu)
}

vars<- c("hydrolab_turbidity_ntu", 
         "sentinel_acolite_turbidity_mean_fnu",
         "sentinel_gee_turbidity_mean_ntu",
         "landsat_acolite_turbidity_mean_fnu",
         "tss", 
         "on_site_probe_turbidity",
         "in_lab_probe_turbidity",
         "phosphrous",
         "nitrate",
         "ammonia",
         "on_site_pH",
         "in_lab_probe_pH",
         "hydrolab_pH",
         "on_site_temp",
         "hydrolab_temp",
         "hydrolab_spec_conductance_mS_cm",
         "hydrolab_dissolved_oxygen_percent",
         "light_absorbance_645",
         "plate_method_abrp",
         "plate_method_ec",
         "idexx_method_ec",
         "idexx_method_abrp",
         "correction_flo_ent",
         "correction_yel_tc")







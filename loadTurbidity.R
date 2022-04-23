loadTurbidity<- function(){
  hydro<- read_sheet("https://docs.google.com/spreadsheets/d/18TUgYYC6s6ZbgMIdV4XYZ72J_nrS0BfzQUxzfqFy2YY/edit#gid=0")
  tss<- read_sheet("https://docs.google.com/spreadsheets/d/1-FOOyM14op9ojekoXiJ7Lx3qxvw4_lSBugtlzLVjdjU/edit#gid=0")
  hydro<- hydro %>%  
    mutate(hydrolab_turbidity_ntu = as.numeric(unlist(hydrolab_turbidity_ntu)),
           sentinel_acolite_turbidity_mean_fnu = as.numeric(unlist(sentinel_acolite_turbidity_mean_fnu)),
           landsat_acolite_turbidity_mean_fnu = as.numeric(unlist(landsat_acolite_turbidity_mean_fnu)),
           on_site_probe_turbidity = as.numeric(unlist(on_site_probe_turbidity)),
           in_lab_probe_turbidity = as.numeric(unlist(in_lab_probe_turbidity)),
           sentinel_gee_turbidity_mean_ntu = as.numeric(unlist(sentinel_gee_turbidity_mean_ntu)),
           phosphrous = as.numeric(unlist(p)),
           nitrite = as.numeric(unlist(no3)),
           ammonia = as.numeric(unlist(nh3)))
  tss<- tss %>%
    group_by(date_sample, sites)%>%
    summarise(tss = mean(tss_g_l, na.rm = T))
  hydro %>%
    select(date_sample,
           sites,
           hydrolab_turbidity_ntu,
           sentinel_acolite_turbidity_mean_fnu,
           landsat_acolite_turbidity_mean_fnu,
           on_site_probe_turbidity,
           in_lab_probe_turbidity,
           sentinel_gee_turbidity_mean_ntu,
           phosphrous,
           nitrite,
           ammonia) %>%
    left_join(tss, c("date_sample", "sites")) 
}

vars<- c("hydrolab_turbidity_ntu", 
         "sentinel_acolite_turbidity_mean_fnu",
         "landsat_acolite_turbidity_mean_fnu","tss", 
         "on_site_probe_turbidity",
         "in_lab_probe_turbidity",
         "sentinel_gee_turbidity_mean_ntu",
         "phosphrous",
         "nitrite",
         "ammonia")











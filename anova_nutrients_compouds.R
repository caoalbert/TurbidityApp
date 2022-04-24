



pi<- loadArg() %>% 
  inner_join(loadFib(), c("sites", "date_sample")) %>%
  mutate(Plate = cat_antibiotics,
         IDEXX = cat_ecoli_resistant)


to_join<- pi %>%
  select(sites, 
         date_sample,
         percent_resistant,
         without_ab_conc) %>%
  rename(plate_method_abrp = percent_resistant,
         plate_method_tc = without_ab_conc)
df<- loadTurbidity() %>% 
  left_join(to_join, c("sites", "date_sample"))


  
df_cat<- within(df,{ammonia_cat<- NA
                    ammonia_cat[ammonia < 0]<- "negative"
                    ammonia_cat[ammonia > 0]<- "positive"
                    phosphrous_cat<- NA
                    phosphrous_cat[phosphrous < 0.5]<- "low"
                    phosphrous_cat[phosphrous > 0.5]<- "high"
                    plate_method_tc_cat<- NA
                    plate_method_tc_cat[plate_method_tc <= 100]<- "low"
                    plate_method_tc_cat[plate_method_tc > 100]<- "high"
                    plate_method_abrp_cat<- NA
                    plate_method_abrp_cat[plate_method_abrp == 0]<- "zero"
                    plate_method_abrp_cat[plate_method_abrp > 0]<- "non-zero"})

m1<- aov(on_site_probe_turbidity ~ phosphrous_cat+as.factor(nitrite)+ammonia_cat+plate_method_tc_cat+plate_method_abrp_cat, df_cat)
summary(m1)


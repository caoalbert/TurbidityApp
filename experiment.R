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

colnames(pi)



ggplot(pi, aes(sites, without_ab_conc))+
  geom_boxplot()




df_flo<- fib() %>% 
  group_by(sites) %>% 
  summarise(mean_tcesbl = mean(correction_flo_tc_esbl),
            mean_flotc = mean(correction_flo_tc))
a<- as.data.frame(cbind(df_flo$sites, df_flo$mean_flotc, rep("Flo_TC", nrow(df_flo))))
b<- as.data.frame(cbind(df_flo$sites, df_flo$mean_tcesbl, rep("Flo_TC_ESBL", nrow(df_flo))))
df_flo2<- rbind(a,b)
colnames(df_flo2)<- c("Sites", "Coliform", "Type")
df_flo2<- df_flo2 %>%
  mutate(Coliform = as.numeric(Coliform))

coords<- coords %>%
  select(Sites, Lat, Long)
saveRDS(coords, "coordinates.rda")

leaflet(coords) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(lng = ~Long, lat = ~Lat, label = ~Sites, icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons8/ios7/512/Travel-Beach-icon.png',iconSize = c(24, 24)))
1

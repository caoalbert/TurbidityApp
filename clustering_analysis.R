
df<- loadTurbidity()
df2<- df %>% 
  select(on_site_probe_turbidity, sentinel_acolite_turbidity_mean_fnu)
df2<- na.omit(df2)
df2<- scale(df2)

output1<- fviz_nbclust(df2, kmeans, method = "wss")

a<- kmeans(df2, 3, 25)

output2<- fviz_cluster(a, df2)+
  theme_bw()+
  ggtitle("Clusters")

df3<- as.data.frame(cbind(df2, a$cluster))
colnames(df3)[3]<- "cluster"
df4<- df3 %>%
  dplyr::filter(cluster == 3)
cluster_model<- lm(df4, formula = sentinel_acolite_turbidity_mean_fnu~on_site_probe_turbidity)



output3<- ggplot(df4, aes(x = sentinel_acolite_turbidity_mean_fnu, y = on_site_probe_turbidity))+
  geom_point()+
  theme_bw()+
  ggtitle("Regression in the Largest Cluster")+
  geom_smooth(method = "lm", se = F)

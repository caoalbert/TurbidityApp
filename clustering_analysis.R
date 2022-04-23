library(factoextra)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
df_cluster_analysis<- loadTurbidity() %>% 
  select(on_site_probe_turbidity, sentinel_acolite_turbidity_mean_fnu) %>%
  na.omit() %>% 
  scale() %>%
  as.data.frame()


plot_select_k<- fviz_nbclust(df_cluster_analysis, kmeans, method = "wss")
kmeans_m1<- kmeans(df_cluster_analysis, 3, 25)
plot_clusters<- fviz_cluster(kmeans_m1, df_cluster_analysis)+
  theme_bw()+
  ggtitle("Clusters")

df_cluster_analysis<- cbind(df_cluster_analysis, kmeans_m1$cluster) %>%
  rename("cluster" = `kmeans_m1$cluster`) %>%
  dplyr::filter(cluster == 3)


lm_largest_cluster<- lm(df_cluster_analysis, formula = sentinel_acolite_turbidity_mean_fnu~on_site_probe_turbidity)



plot_largest_cluster_regression<- ggplot(lm_largest_cluster, aes(x = sentinel_acolite_turbidity_mean_fnu, y = on_site_probe_turbidity))+
  geom_point()+
  theme_bw()+
  ggtitle("Regression in the Largest Cluster")+
  geom_smooth(method = "lm", se = F)








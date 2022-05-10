library(factoextra)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Clean the Data 
df_cluster_analysis<- loadTurbidity() %>% 
  select(on_site_probe_turbidity, sentinel_acolite_turbidity_mean_fnu) %>%
  na.omit() %>% 
  scale() %>%
  as.data.frame()

# Select number of clusters
plot_select_k<- fviz_nbclust(df_cluster_analysis, kmeans, method = "wss")

# Build clustering model
kmeans_m1<- kmeans(df_cluster_analysis, 4, 25)

# Visualize the clsuter
plot_clusters<- fviz_cluster(kmeans_m1, df_cluster_analysis)+
  theme_bw()+
  ggtitle("Clusters")

# Prepare the data for regression
df_cluster_analysis<- cbind(df_cluster_analysis, kmeans_m1$cluster) %>%
  rename("cluster" = `kmeans_m1$cluster`) %>%
  dplyr::filter(cluster == 3)

# Regression against the largest cluster
lm_largest_cluster<- lm(df_cluster_analysis, formula = sentinel_acolite_turbidity_mean_fnu~on_site_probe_turbidity)

# Visualize the regression
plot_largest_cluster_regression<- ggplot(lm_largest_cluster, aes(x = on_site_probe_turbidity, y = sentinel_acolite_turbidity_mean_fnu))+
  geom_point()+
  theme_bw()+
  ggtitle("Regression in the Largest Cluster")+
  geom_smooth(method = "lm", se = F)








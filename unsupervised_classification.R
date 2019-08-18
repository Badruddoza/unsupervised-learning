rm(list=ls())
#install.packages("factoextra")
#install.packages("cluster")
#install.packages("magrittr")
library("cluster")
library("factoextra")
library("magrittr")
#Load  and prepare the data
data("USArrests")
my_data <- USArrests %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables
# View the firt 3 rows
head(my_data, n = 3)
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
library("factoextra")
fviz_nbclust(my_data, kmeans, method = "gap_stat")
set.seed(123)
#k-means clustering
km.res <- kmeans(my_data, 3, nstart = 25)
# Visualize
library("factoextra")
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Compute PAM clustering
library("cluster")
pam.res <- pam(my_data, 3)
# Visualize
fviz_cluster(pam.res)
# Compute hierarchical clustering
res.hc <- USArrests %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

## MODEL VALIDATION
gradient.color <- list(low = "steelblue",  high = "white")
iris[, -5] %>%    # Remove column 5 (Species)
  scale() %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)
set.seed(123)
# Compute
library("NbClust")
res.nbclust <- USArrests %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all") 
# Visualize
library(factoextra)
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
set.seed(123)
# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- iris[, -5] %>%
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)
# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
fviz_silhouette(res.hc)
# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

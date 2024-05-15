library(readxl)
library(dplyr)
library(fpc)
library(MASS)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(flexclust)
library(factoextra)
library(NbClust)
library(caret)
library(ggfortify)
library(FactoMineR)
library(cluster)
library(readr)

######################################################## 1st Subtask ####################################################################
# Read the csv file by opening it. 
whitewine <- read_xlsx("C:/Users/Tharushi/IIT/IIT/2nd yr/2nd semester/ML/CW/R/ML_CW/Whitewine_v6.xlsx")%>%
  janitor::clean_names()
View(whitewine)

# Delete the last column
whitewine <- whitewine[, -ncol(whitewine)]
#View(whitewine)
#summary(whitewine)

# Create a boxplot of the data before outlier removal
boxplot(whitewine, main = "Before Outlier Removal", outcol="red")

# Before removing the outliers, count the rows and columns and print the results.

rows_of_dataset <- nrow(whitewine)
columns_of_dataset <- ncol(whitewine)

# print the results
cat("Rows to delete before outliers   :", rows_of_dataset, "\n")
cat("columns to delete before outliers:", columns_of_dataset, "\n")

# Create a function to remove outliers from a single column using the boxplot method.

outliers_remove <- function(x) {
  bp <- boxplot.stats(x)$stats
  x[x < bp[1] | x > bp[5]] <- NA
  return(x)
}

# Apply the function to each data frame column.

whithout_outliers_in_dataset <- apply(whitewine, 2, outliers_remove)

# Remove any rows with missing values

whithout_outliers_in_dataset <- na.omit(whithout_outliers_in_dataset)

# After removing the outliers, count the rows and columns and print the results.

rows_of_dataset_after_delete_outliers <- nrow(whithout_outliers_in_dataset)
columns_of_dataset_after_delete_outliers <- ncol(whithout_outliers_in_dataset)

# print the results
cat("After deleting outliers, Number of Rows   :", rows_of_dataset_after_delete_outliers, "\n")
cat("After deleting outliers, Number of Columns:", columns_of_dataset_after_delete_outliers, "\n")

# Create a boxplot of the data after outlier removal
boxplot(whithout_outliers_in_dataset,main = "After Outlier Removal")

# Scale up the data set.

# Scale dataset to center and scale each variable to have unit variance
scaled_whitewine_dataset <- scale(whithout_outliers_in_dataset)
head(scaled_whitewine_dataset)

# Create a boxplot to visualize the distribution of each variable in the scaled dataset
boxplot(scaled_whitewine_dataset)
# Method 1:Nbclust method

# Set the random seed to 1234 for reproducibility
set.seed(1234)

# Determine the optimal number of clusters using the NbClust function
# NbClust calculates several indices and methods to determine the optimal number of clusters
NBcluster <- NbClust(scaled_whitewine_dataset, min.nc = 2,max.nc = 10, method = "kmeans")
#table(NBcluster$Best.n[1,])

# Create a barplot to visualize the number of times each number of clusters is chosen as optimal
barplot(table(NBcluster$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen")

# Visualize the optimal number of clusters using different methods
any(is.na(scaled_whitewine_dataset))
# Method 2: Elbow Method(Within-cluster sum of squares (WSS) method)
# This method calculates the total wss for different numbers of clusters and plots it to help determine the optimal number of clusters
fviz_nbclust(scaled_whitewine_dataset,kmeans,method = "wss")

# Method 3: Silhouette method
# This method calculates the average silhouette width for different numbers of clusters and plots it to help determine the optimal number of clusters
fviz_nbclust(scaled_whitewine_dataset,kmeans,method = "silhouette")
print(fviz_nbclust)

# Method 4: Gap statistic method
# This method calculates the gap statistic for different numbers of clusters and plots it to help determine the optimal number of clusters
fviz_nbclust(scaled_whitewine_dataset,kmeans,method = "gap_stat")



# Perform k-means clustering with k=2
k2 <-kmeans(scaled_whitewine_dataset, 2)
# Display the k-means clustering results
k2
# Visualize the clustering results with autoplot
autoplot(k2,scaled_whitewine_dataset,frame=TRUE)

# Extract relevant information when k=2

# Cluster centers
cluster_centers <- k2$centers

# Clustered results
cluster_assignments <- k2$cluster 

# Between-cluster sum of squares
BSSk2 <- k2$betweenss 

# Within-cluster sum of squares
WSSk2 <- k2$tot.withinss 

# Total sum of squares
TSSk2 <- BSSk2 + WSSk2 

# BSS/TSS ratio
BSS_TSS_ratiok2 <- BSSk2 / TSSk2 

# Percentage of variance explained
percent_vark2 <- round(BSS_TSS_ratiok2 * 100, 3) 

# Output results
cat("Cluster centers:\n", cluster_centers, "\n\n")
cat("Cluster assignments:\n", cluster_assignments, "\n\n")
cat("BSS/TSS ratio: ", round(BSS_TSS_ratiok2, 3), "\n\n")
cat("BSS: ", round(BSSk2, 3), "\n\n")
cat("WSS: ", round(WSSk2, 3), "\n\n")
cat("Percentage of variance explained: ", percent_vark2, "%\n")



# Perform k-means clustering with k=3
k3 <-kmeans(scaled_whitewine_dataset, 3)
# Display the k-means clustering results
k3
# Visualize the clustering results with autoplot
autoplot(k3,scaled_whitewine_dataset,frame=TRUE)

# Extract relevant information when k=3

# Cluster centers
cluster_centers <- k3$centers

# Clustered results
cluster_assignments <- k3$cluster 

# Between-cluster sum of squares
BSSk3 <- k3$betweenss 

# Within-cluster sum of squares
WSSk3 <- k3$tot.withinss 

# Total sum of squares
TSSk3 <- BSSk3 + WSSk3 

# BSS/TSS ratio
BSS_TSS_ratiok3 <- BSSk3 / TSSk3 

# Percentage of variance explained
percent_vark3 <- round(BSS_TSS_ratiok3 * 100, 3) 

# Output results
cat("Cluster centers:\n", cluster_centers, "\n\n")
cat("Cluster assignments:\n", cluster_assignments, "\n\n")
cat("BSS/TSS ratio: ", round(BSS_TSS_ratiok3, 3), "\n\n")
cat("BSS: ", round(BSSk3, 3), "\n\n")
cat("WSS: ", round(WSSk3, 3), "\n\n")
cat("Percentage of variance explained: ", percent_vark3, "%\n")


# Set the number of clusters to 2
k <- 2

# Fit a k-means model to the scaled Vehicles dataset with 2 clusters and 25 random initializations
part01_kmeans_model_2 <- kmeans(scaled_whitewine_dataset, centers = k, nstart = 25)

# Compute the silhouette plot for the k-means model
silhouette_plot2 <- silhouette(part01_kmeans_model_2$cluster, dist(whithout_outliers_in_dataset))

# Calculate the average silhouette width for the k-means model
avg_silhouette_width <- mean(silhouette_plot2[, 3])

# Plot the silhouette plot with a title, x-axis label, and y-axis label
plot(silhouette_plot2, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# Add a vertical line to the plot at the average silhouette width
abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")


# Set the number of clusters to 3
k <- 3
# Fit a k-means model to the scaled Vehicles dataset with 2 clusters and 25 random initializations
part01_kmeans_model_3 <- kmeans(scaled_whitewine_dataset, centers = k, nstart = 25)

# Compute the silhouette plot for the k-means model
silhouette_plot3 <- silhouette(part01_kmeans_model_3$cluster, dist(scaled_whitewine_dataset))

# Calculate the average silhouette width for the k-means model
avg_silhouette_width <- mean(silhouette_plot3[, 3])

# Plot the silhouette plot with a title, x-axis label, and y-axis label
plot(silhouette_plot3, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# Add a vertical line to the plot at the average silhouette width
abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")





######################################################## 2nd Subtask ####################################################################


# Perform Principal Component Analysis (PCA)
pca <- prcomp(scaled_whitewine_dataset)

# Print summary of PCA results
print(summary(pca))

# Calculate variance explained by each principal component
pca_var <- pca$sdev^2

# Calculate proportion of variance explained by each principal component
pca_var_prop <- pca_var / sum(pca_var)

# Calculate cumulative proportion of variance explained
pca_var_cumprop <- cumsum(pca_var_prop)

# Plot the cumulative proportion of variance explained by each principal component
plot(pca_var_cumprop, xlab = "Principal Component", ylab = "Explained Cumulative Proportion of Variance",ylim = c(0, 1), type = "b")

# Perform PCA transformation on the original data
pca_trans <- predict(pca, newdata = scaled_whitewine_dataset)

# Select principal components that explain more than 85% of the variance
selected_pcs <- which(pca_var_cumprop > 0.85)

# Extract transformed data using selected principal components
transformed_data <- pca_trans[, selected_pcs]

# Create a boxplot to visualize the distribution of the transformed data
boxplot(transformed_data)

View(transformed_data)

# Set seed for reproducibility
set.seed(1234)

# Determine the optimal number of clusters using NbClust after PCA transformation
NBcluster <- NbClust(transformed_data, min.nc = 2,max.nc = 10, method = "kmeans")

# Create a barplot to visualize the number of times each number of clusters is chosen as optimal after PCA
barplot(table(NBcluster$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen after PCA2")

# Visualize the optimal number of clusters using the elobow (within-cluster sum of squares(WSS)) method after PCA
fviz_nbclust(transformed_data,kmeans,method = "wss")

# Visualize the optimal number of clusters using the silhouette method after PCA
fviz_nbclust(transformed_data,kmeans,method = "silhouette")

# Visualize the optimal number of clusters using the gap statistic method after PCA
fviz_nbclust(transformed_data,kmeans,method = "gap_stat")



# Set seed for reproducibility
set.seed(1234)

# Perform k-means clustering with k=2
kmeans_model_2 <- kmeans(transformed_data, centers = 2, nstart = 25)

# Print the k-means clustering results
print(kmeans_model_2)

# Visualize the k-means clustering results
autoplot(kmeans_model_2, transformed_data, frame = TRUE)

# Print results for k=2
cat("For k=2:\n")

# Calculate within-cluster sum of squares (WSS)
wss_2 <- sum(kmeans_model_2$withinss)
cat("Within-cluster sum of squares (WSS): ", wss_2, "\n")

# Calculate between-cluster sum of squares (BSS)
bss_2 <- sum(kmeans_model_2$size * dist(rbind(kmeans_model_2$centers, colMeans(transformed_data)))^2)
cat("Between-cluster sum of squares (BSS): ", bss_2, "\n")

# Calculate total sum of squares (TSS)
tss_2 <- sum(dist(transformed_data)^2)
cat("Total sum of squares (TSS): ", tss_2, "\n")

# Calculate ratio of BSS to TSS
bss_tss_ratio_2 <- bss_2 / tss_2
cat("Ratio of BSS to TSS: ", bss_tss_ratio_2, "\n\n")



# Set seed for reproducibility
set.seed(1234)

# Perform k-means clustering with k=3
kmeans_model_3 <- kmeans(transformed_data, centers = 3, nstart = 25)

# Print the k-means clustering results
print(kmeans_model_3)

# Visualize the k-means clustering results
autoplot(kmeans_model_3, transformed_data, frame = TRUE)

# Print results for k=3
cat("For k=3:\n")

# Calculate within-cluster sum of squares (WSS)
wss_3 <- sum(kmeans_model_3$withinss)
cat("Within-cluster sum of squares (WSS): ", wss_3, "\n")

# Calculate between-cluster sum of squares (BSS)
bss_3 <- sum(kmeans_model_3$size * dist(rbind(kmeans_model_3$centers, colMeans(transformed_data)))^2)
cat("Between-cluster sum of squares (BSS): ", bss_3, "\n")

# Calculate total sum of squares (TSS)
tss_3 <- sum(dist(transformed_data)^2)
cat("Total sum of squares (TSS): ", tss_3, "\n")

# Calculate ratio of BSS to TSS
bss_tss_ratio_3 <- bss_3 / tss_3
cat("Ratio of BSS to TSS: ", bss_tss_ratio_3, "\n")



# Set the number of clusters
k <- 2

# Perform k-means clustering with k=2
kmeans_model_2 <- kmeans(transformed_data, centers = k, nstart = 25)

# Calculate silhouette widths
silhouette_plot <- silhouette(kmeans_model_2$cluster, dist(transformed_data))

# Calculate average silhouette width
avg_silhouette_width <- mean(silhouette_plot[, 3])

# Plot silhouette plot
plot(silhouette_plot, 
     main = paste0("Silhouette Plot for k =", k), 
     xlab = "Silhouette Width",                   
     ylab = "Cluster",                            
     border = NA)                               

# Add a vertical line representing the average silhouette width
abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")



# Set the number of clusters
k <- 3

# Perform k-means clustering with k=3
kmeans_model_3 <- kmeans(transformed_data, centers = k, nstart = 25)

# Calculate silhouette widths
silhouette_plot <- silhouette(kmeans_model_3$cluster, dist(transformed_data))

# Calculate average silhouette width
avg_silhouette_width <- mean(silhouette_plot[, 3])

# Plot silhouette plot
plot(silhouette_plot, 
     main = paste0("Silhouette Plot for k =", k), 
     xlab = "Silhouette Width",                   
     ylab = "Cluster",                            
     border = NA)                                

# Add a vertical line representing the average silhouette width
abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")





# Define a function to calculate the Calinski-Harabasz index for PCA-transformed data
calinski_harabasz_pca2 <- function(cluster_result, data) {
  
  # Determine the number of clusters (k)
  k2 <- length(unique(cluster_result$cluster))
  
  # Get the number of observations (n)
  n2 <- nrow(data)
  
  # Calculate Between-Cluster Sum of Squares (BSS) and Within-Cluster Sum of Squares (WSS)
  BSS2 <- cluster_result$betweenss
  WSS2 <- cluster_result$tot.withinss
  
  # Calculate the Calinski-Harabasz index
  ch_index2 <- ((n2 - k2) / (k2 - 1)) * (BSS2 / WSS2)
  
  return(ch_index2)
}

# Calculate the Calinski-Harabasz index for k-means clustering result with k=2
ch_index_pca_2 <- calinski_harabasz_pca2(kmeans_model_2, transformed_data)

# Print the Calinski-Harabasz index
cat("The Calinski-Harabasz index for the K-means (k=2) clustering result is:", ch_index_pca_2, "\n")




# Define a function to calculate the Calinski-Harabasz index for PCA-transformed data
calinski_harabasz_pca3 <- function(cluster_result, data) {
  
  # Determine the number of clusters (k)
  k3 <- length(unique(cluster_result$cluster))
  
  # Get the number of observations (n)
  n3 <- nrow(data)
  
  # Calculate Between-Cluster Sum of Squares (BSS) and Within-Cluster Sum of Squares (WSS)
  BSS3 <- cluster_result$betweenss
  WSS3 <- cluster_result$tot.withinss
  
  # Calculate the Calinski-Harabasz index
  ch_index3 <- ((n3 - k3) / (k3 - 1)) * (BSS3 / WSS3)
  
  return(ch_index3)
}

# Calculate the Calinski-Harabasz index for k-means clustering result with k=3
ch_index_pca_3 <- calinski_harabasz_pca3(kmeans_model_3, transformed_data)

# Print the Calinski-Harabasz index
cat("The Calinski-Harabasz index for the K-means (k=3) clustering result is:", ch_index_pca_3, "\n")

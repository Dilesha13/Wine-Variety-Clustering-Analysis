# Wine-Variety-Clustering-Analysis

Project Overview:

  My project focused on clustering white wine data to identify distinct groups based on their chemical properties. This involved data preprocessing, outlier removal, scaling, determining the optimal number of clusters, performing k-means clustering, and conducting Principal Component Analysis (PCA) to enhance the clustering results.


1. Data Preprocessing:
   
      *Data Loading: I started by loading the white wine dataset from an Excel file and cleaned the column names for consistency.
   
      *Outlier Removal: Using a custom function, I removed outliers from the dataset by replacing values outside the interquartile range (IQR) with NA and then removed any rows containing NA values.
   
      *Data Scaling: To ensure all features contributed equally to the clustering, I scaled the data so each feature had a mean of zero and unit variance.

   
2. Determining Optimal Number of Clusters:
   
    I used several methods to determine the optimal number of clusters:

      *NbClust: This method uses various criteria to suggest the optimal number of clusters.
   
      *Elbow Method (WSS): Plotted the within-cluster sum of squares against the number of clusters to find the 'elbow point'.
   
      *Silhouette Method: Calculated average silhouette widths for different numbers of clusters to find the optimal one.
   
      *Gap Statistic: Compared the total within intra-cluster variation for different numbers of clusters with their expected values under null reference distribution of the data.

   
3. K-means Clustering:
   
      *k=2 and k=3 Clusters: I performed k-means clustering for k=2 and k=3 clusters based on the earlier analyses.

      *Cluster Evaluation: Evaluated the clusters using metrics such as within-cluster sum of squares (WSS), between-cluster sum of squares (BSS), total sum of squares (TSS), and the BSS/TSS ratio. I also visualized the clusters using autoplot.

   
4. Cluster Validation:
   
      *Silhouette Analysis: Calculated silhouette widths for k=2 and k=3 to measure how similar an object is to its own cluster compared to other clusters.

      *Calinski-Harabasz Index: This index evaluates the clustering's compactness and separation.

   
6. Principal Component Analysis (PCA):
   
      *PCA Transformation: Performed PCA to reduce the dataset's dimensionality while retaining 85% of the variance.

      *Re-Clustering on PCA-transformed Data: Repeated the clustering analysis on the transformed data to potentially improve clustering performance.

Key Results:
  # Optimal Number of Clusters: The analysis suggested that 2 and 3 clusters were appropriate.
  
  #Clustering Performance: The clusters for k=2 and k=3 were evaluated using silhouette scores and Calinski-Harabasz index, with respective values indicating good cluster separation and compactness.
  
  #Improvement with PCA: The clustering on PCA-transformed data showed slightly better performance metrics, indicating PCA's utility in enhancing clustering quality.

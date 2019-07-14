#---SEGMENTATION AND DESCRIPTOR VARIABLES

seg_vars <- BF[8:25]
des_vars <- BF[2:7]


#---PACKAGE REQUIRED

library(ggplot2)
library(ggfortify)
library(flexclust)
library(data.table)
library(randomForest)
library(nnet)
library(clue)

#---DIMENSION REDUCTION

# Principal Components Analysis
pca_result = pca_function(seg_vars, center = TRUE, scale = TRUE)

# Choosing the optimal number of components
screeplot_function(pca_result)

# Show biplot
biplot_function(pca_result, seg_vars, x = 1, y = 2)

#---DISTANCE-BASED CLUSTERING

# K-means Clustering
kmeans_result = clustering_function(pca_result, n = 4, kmax = 10, nrep = 10, method = "kmeans")
elbow_function(kmeans_result)

# Hard Competitive Learning
hardcl_result = clustering_function(pca_result, n = 4, kmax = 10, nrep = 10, method = "hardcl")
elbow_function(hardcl_result)

# Neural Gas
neuralgas_result = clustering_function(pca_result, n = 4, kmax = 10, nrep = 10, method = "neuralgas")
elbow_function(neuralgas_result)


#---DATA STRUCTURE ANALYSIS

# Run clustering repeatedly for different numbers of clusters on bootstrap replica
boot_kmeans = boot_clust_function(pca_result, 4, method = "kmeans")
boot_hardcl = boot_clust_function(pca_result, 4, method = "hardcl")
boot_neuralgas = boot_clust_function(pca_result, 4, method = "neuralgas")

# Global stability
par(mfrow=c(1,3))
global_stability(boot_kmeans, main = "K-means clustering")
global_stability(boot_hardcl, main = "Hard competitive learning")
global_stability(boot_neuralgas, main = "Neural gas" )

# Segment level stability across solutions
par(mfrow=c(1,1))
slsa_function(kmeans_result)    
slsa_function(hardcl_result)    
slsa_function(neuralgas_result) 

# Segment level stability within solutions
slsw_function(pca_result, n = 4, k = 6, method = "neuralgas", main = "Six-segment solution")

#---PROFILING SEGMENTS

profile_plot(neuralgas_result, k = 6, seg_vars)

#---DESCRIBING SEGMENTS

par(mfrow=c(1,1))
mosaic_plot(neuralgas_result, k = 6, des_vars$Gender, main = "Gender", xlab = "Cluster number")
mosaic_plot(neuralgas_result, k = 6, des_vars$Age, main = "Age", xlab = "Cluster number")
mosaic_plot(neuralgas_result, k = 6, des_vars$Occupation, main = "Occupation", xlab = "Cluster number")
mosaic_plot(neuralgas_result, k = 6, des_vars$City_Category, main = "City category", xlab = "Cluster number")
mosaic_plot(neuralgas_result, k = 6, des_vars$Stay_In_Current_City_Years, main = "Stay in current city years", xlab = "Cluster number")
mosaic_plot(neuralgas_result, k = 6, des_vars$Marital_Status, main = "Marital status", xlab = "Cluster number")

#---SEGMENT PREDICTION

# Divide data into 60% training, 20% validation and 20% test set
des_vars$Segment = as.factor(neuralgas_result[["6"]]@cluster)
data_set = split_function(des_vars, v = 0.2, t = 0.2)

# Bagging and Random Forests
rf = rf_tuning(Segment ~ ., train = data_set$train, valid = data_set$validation, segment = "Segment", p = 6)
im <- importance(rf$Models[[2]])
im <- data.frame(var = c("Gender", "Age", "Occupation", "City category", "Stay in current city years", "Marital status"),
                 mda = im[,7])
ggplot(im) + 
  geom_bar(aes(y = mda, x = var, fill = var), stat = "identity", fill="steelblue") +
  ylab("\nMean decrease of predictive accuracy (%)") +
  coord_flip() +
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_text(size = 10))
  

# Multinomial Logistic Classification
mlc = multinom_pred(Segment ~ ., train = data_set$train, valid = data_set$validation, segment = "Segment")

# Misclassification error rate on test set of final model
model = randomForest(Segment ~ ., data = rbind(data_set$train, data_set$validation), mtry = 2, ntree = 500, importance = T)
1 - mean(predict(model, data_set$test) == data_set$test$Segment) 

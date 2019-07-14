# Packages Required

library(ggplot2)
library(ggfortify)
library(flexclust)
library(data.table)
library(randomForest)
library(nnet)
library(clue)

# Principal Components Analysis

pca_function = function(data, center = TRUE, scale = TRUE) {
  # Funtion to perform PCA
  # data: set of segmentation variables
  pca_result = prcomp(data, center = center, scale = scale)  # PCA with centered normalized variables
  return(pca_result)
}

screeplot_function = function(pca_result) {
  # Function to select the optimal number of components
  # pca_result must be the outcome of pca_function() or prcomp()
  
  var_expl = pca_result$sdev^2                            # Variance explained per component
  prop_var_expl = var_expl/sum(var_expl)                  # Proportion variance explained per component
  cumul_prop_var_expl = cumsum(prop_var_expl)             # Cumulative proportion variance explained
  
  par(mfrow=c(1,2))
  plot( prop_var_expl, type="b" , xlab="Number of components",        # Scree plot
        ylab="Proportion of variance explained", ylim=c(0,1), pch = 20)
  plot( cumul_prop_var_expl, type="b" , xlab="Number of components",  # Cumulative proportion of variance explained plot
        ylab="Cumulative proportion of variance explained", ylim=c(0,1), pch = 20)
  
  nElem = length(cumul_prop_var_expl)
  ratios = prop_var_expl[2:(nElem-1)]/prop_var_expl[3:(nElem)]        # Choose the largest one
  nComponents_selected = which.max(ratios) + 2                        # Optimal number of components
  
  l = list("Ratios (from PC2/PC3)" = ratios, "Optimal number of components" = nComponents_selected)
  return(l)
}

biplot_function = function(pca_result, data, x = 1, y = 2) {
  # Biplot
  # pca_result must be the outcome of pca_function() or prcomp()
  # data: set of segmentation variables 
  # x is the component on x-axis, y is the component on y-axis
  autoplot(pca_result, data = data, x = x, y = y, colour = "dark grey", 
           loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, xlim = c(-0.015, 0.075))
}

# Centroid-Based Clustering

clustering_function = function(pca_result, n, kmax = 10, nrep = 10, method = "kmeans", seed = NULL) {
  # pca_result must be the outcome of pca_function() or prcomp()
  # n: selected number of components
  # kmax: maximum number of clusters; 
  # nrep: for each value of k run clustering nrep times and keep only the best solution
  # method: choose between "kmeans", "hardcl" and "neuralgas"
  # seed: set.seed() is made before process
  data.pca = pca_result$x[,1:n] 
  kmax = kmax      
  clust_result = stepFlexclust(data.pca, 2:kmax, nrep = 10, verbose = FALSE, method = method, FUN = "cclust", seed = seed)
  clust_result = relabel(clust_result)
  return(clust_result)
}

elbow_function = function(clust_result) {
  # clust_result must be passed to clustering_function() or stepFlexclust()
  sum_wit_cldist = numeric()
  kmax = max(clust_result@k)
  for (i in 1:(kmax - 1))
  {
    sum_wit_cldist[i+1] = info(clust_result[[i]], "distsum")
  }
  sum_wit_cldist[1] = clust_result@totaldist
  prop_expl = (clust_result@totaldist - sum_wit_cldist)/clust_result@totaldist
  sum_wit_cldist  # Within-cluster sum of squares
  prop_expl       # Proportion explained
  
  par(mfrow=c(1,2))
  plot(1:kmax, sum_wit_cldist, type = "b", pch = 20, xlab = "Number of clusters", ylab = "Within-cluster sum of squares") # Scree plot
  plot(1:kmax, prop_expl, type = "b", pch = 20, xlab = "Number of clusters", ylab = "Proportion of variance explained") # Proportion explained plot
  
  ratios = sum_wit_cldist[2:(length(sum_wit_cldist)-1)]/sum_wit_cldist[3:(length(sum_wit_cldist))]  # Choose the largest one
  suggested = which.max(ratios) + 2  # Suggested number of clusters
  
  l = list("Ratios (from k=2/k=3)" = ratios, "Suggested number of clusters" = suggested)
  return(l)
}

# Data Structure Analysis

boot_clust_function = function(pca_result, n, kmax = 10, nrep = 10, nboot = 100, method = "kmeans", seed = NULL){
  # Run clustering repeatedly for different numbers of clusters on bootstrap replica
  # n: selected number of components
  # nboot: number of bootstrap pairs of partitions 
  # nrep: for each boostrap pair run clustering nrep times and keep only the best solution
  # method: choose between "kmeans", "hardcl" and "neuralgas"
  # seed: set.seed() is made before process
  boot_clust <- bootFlexclust(pca_result$x[,1:n], 2:kmax, nrep = nrep, nboot = nboot, seed = seed, method = method, FUN = "cclust")
}

global_stability = function(boot_clust, main = "") {
  # Global stability
  # boot_clust must be passed to boot_clust_function() or bootFlexclust()
  # main: plot title
  plot(boot_clust, xlab = "Number of segments", ylab = "Adjusted Rand index", main = main)
  return(summary(boot_clust))
}

slsa_function = function(clust_result) {
  # Segment level stability across solutions
  # kmeans_result must be passed to kmeans_function() or stepFlexclust()
  slsa = slsaplot(clust_result)
  avg_stab = numeric()
  l = length(slsa)
  for (i in 1:l) {
    avg_stab[i] = mean(1 - slsa[[i]])
  }
  avg_stab = t(avg_stab)
  colnames(avg_stab) = (length(slsa[[l]]) - ncol(avg_stab) + 1):length(slsa[[l]])
  list = list("Average stability across solutions" = avg_stab)
  return(list)
}

slsw_function = function(pca_result, n, k, nsamp = 100, nrep = 10, method = "kmeans", seed = NULL, main = "") {
  # Segment level stability within solutions
  # pca_result must be the outcome of pca_function() or prcomp()
  # n: selected number of components
  # k: selected number of clusters
  # nsamp: number of bootstrap pairs drawn
  # nrep: for each boostrap pair run k-means clustering nrep times and keep only the best solution
  # method: choose between "kmeans", "hardcl" and "neuralgas"
  # seed: set.seed() is made before process
  # main: plot title
  data.pca = pca_result$x[,1:n]
  stab = slswFlexclust(data.pca, kmeans_result[[k-1]], nsamp = nsamp, nrep = nrep, method = "kmeans", FUN = "cclust", seed = seed)
  plot(stab, ylim = 0:1, xlab = "Number of segments", ylab = "Segment stability", main = main)
  return(summary(stab))
}


# Profiling Clusters

profile_plot = function(clust_result, k, data) {
  # clust_result must be passed to clustering_function() or stepFlexclust()
  # k: selected number of clusters
  # data: set of segmentation Variables
  vclust <- hclust(dist(t(data)))
  data <- data[,rev(vclust$order)]
  global_mean <- colMeans(data) # Compute global mean
  data$cluster <- clust_result[[k-1]]@cluster
  cluster_mean <- aggregate(. ~ cluster, data = data, mean) # Mean of each cluster for each variables
  percentage <- round((clust_result[[k-1]]@clusinfo$size)/sum(clust_result[[k-1]]@clusinfo$size),2)*100
  newdata <- melt(cluster_mean,id.vars = "cluster")
  
  for (i in 1:k) {
    newdata$cluster[newdata$cluster == i] <- paste(paste("Cluster",i),paste0("(",percentage[i],"%",")"))
  }
  
  ggplot(newdata) + 
    geom_bar(aes(y = value, x = variable, fill = variable), stat = "identity") +    
    geom_point(aes(y = rep(global_mean, each = k), x = variable), colour = "red") + 
    facet_wrap(~ cluster) +
    scale_fill_viridis_d() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
}

# Describing Clusters

mosaic_plot = function(clust_result, k, x, main = "", xlab = "Cluster number") {
  # clust_result must be passed to clustering_function() or stepFlexclust()
  # k: selected number of clusters
  # x: descriptor Variable
  if (main == "") {
    mosaicplot(table(clust_result[[k-1]]@cluster, x), shade = TRUE, main = deparse(substitute(x)), xlab = xlab, ylab = '')
  }
  else {
    mosaicplot(table(clust_result[[k-1]]@cluster, x), shade = TRUE, main = main, xlab = xlab, ylab = '')
  }
}

# Cluster Prediction

split_function = function(data, v, t, seed = NULL) {
  # Function to split data into training set and test set
  # data: set of descriptor variables and segment variable
  # v: percentage of validation set
  # t: percentage of test set
  # seed: set.seed() is made before process
  set.seed(seed)
  n = nrow(data)
  length = 1:nrow(data)
  valid = sample(length, n*v)
  test = sample(length[-valid], n*t)
  valid_set = data[valid,]
  test_set = data[test,]
  train_set = data[-c(test, valid),]
  l = list(train = train_set, validation = valid_set, test = test_set)
  return(l)
}


rf_tuning = function(formula, train, valid, segment = "Segment", p, ntree = 500, seed = NULL) {
  # Function for tunning parameter in Random Forests
  # formula: formula describing models to be fitted
  # train: training set to fit models 
  # valid: valiadtion set to estimate test misclassification error rate
  # segment: name of segment variable in training and validation set
  # p: number of predictors
  # ntree: number of trees to grow
  # seed: set.seed() is made before process
  set.seed(seed)
  
  model = list()
  # Fitting models with m from 1 to p
  for (i in 1:p) {
    model[[i]] = randomForest(formula = formula, data = train, mtry = i, ntree = 500, importance = T)
  }
  
  pred = list()
  # Predict using models and validation set
  for (i in 1:p) {
    pred[[i]] = predict(model[[i]], valid)
  }  
  
  error = numeric()
  # Calculate test misclassification error rate
  for (i in 1:p) {
    error[i] = 1 - mean(pred[[i]] == valid[[segment]])
  }
  
  m = which.min(error) # Optimal number of predictors
  list = list("Test misclassification error rate (from m = 1 to m = p)" = error, 
           "Optimal number of predictors considered at each split" = m,
           "Models" = model)
  return(list)
}

multinom_pred = function(formula, train, valid, segment = "Segment") {
  model = multinom(formula, data = train, trace = 0)
  pred = predict(model, valid)
  error = 1 - mean(pred == valid[[segment]])
  list = list("Test misclassification error rate" = error, "Model" = model)
  return(list)
}

# Saving functions
save(biplot_function, boot_clust_function, clustering_function, elbow_function, global_stability, mosaic_plot, multinom_pred, pca_function, profile_plot, rf_tuning, screeplot_function, slsa_function, slsw_function, split_function, file = "Function.Rdata")


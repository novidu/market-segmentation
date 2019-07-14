par(mfrow=c(1,3))
global_stability(boot_AMD5, main = "AMD")
global_stability(boot_kmeans, main = "Black Friday")
global_stability(boot_rs, main = "RS" )

par(mfrow=c(1,1))
elbow_function(AMD5_kmeans)
elbow_function(rs_kmeans)
elbow_function(kmeans_result)

plot(iris_slsw_3)
plot(AMD5_slsw_5)

loadfonts()
pdf("41.pdf", height=5, width=9)
par(mfrow=c(1,2))
plot( prop_var_expl, type="b" , xlab="Number of components",        # Scree plot
      ylab="Proportion of variance explained", ylim=c(0,1), pch = 20)
plot( cumul_prop_var_expl, type="b" , xlab="Number of components",  # Cumulative proportion variance explained plot
      ylab="Cumulative proportion of variance explained", ylim=c(0,1), pch = 20)
dev.off()

pdf("42.pdf", height=5, width=9)
elbow_function(kmeans_result)
dev.off()

pdf("43.pdf", height=5, width=8)
global_stability(boot_kmeans)
dev.off()

pdf("44.pdf", height=7, width=10)
slsa_function(kmeans_result)  
dev.off()

pdf("45a.pdf", height=7, width=6)
global_stability(boot_hardcl)  
dev.off()

pdf("45b.pdf", height=7, width=6)
global_stability(boot_neuralgas)  
dev.off()

pdf("46a.pdf", height=6, width=6)
slsa_function(neuralgas_result)  
dev.off()

pdf("46b.pdf", height=7, width=10)
slsa_function(neuralgas_result)  
dev.off()

pdf("47.pdf", height=8, width=10)
profile_plot(neuralgas_result, k = 6, seg_vars) 
dev.off()

pdf("48a.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$Gender, main = "Gender", xlab = "Cluster number")
dev.off()

pdf("48b.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$Age, main = "Age", xlab = "Cluster number") 
dev.off()

pdf("48c.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$Occupation, main = "Occupation", xlab = "Cluster number")
dev.off()

pdf("48d.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$City_Category, main = "City category", xlab = "Cluster number")
dev.off()

pdf("48e.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$Stay_In_Current_City_Years, main = "Stay in current city years", xlab = "Cluster number")
dev.off()

pdf("48f.pdf", height=6, width=6)
mosaic_plot(neuralgas_result, k = 6, des_vars$Marital_Status, main = "Marital status", xlab = "Cluster number")
dev.off()

pdf("49.pdf", height=3, width=6)
ggplot(im) + 
  geom_bar(aes(y = mda, x = var, fill = var), stat = "identity", fill="steelblue") +
  ylab("\nMean decrease of predictive accuracy (%)") +
  coord_flip() +
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_text(size = 10))
dev.off()


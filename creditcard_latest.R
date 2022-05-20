
# Loading required libraries

library(animation) # for creating animation represenation
library(ggplot2) # Data visualization
library(dendextend) # used to extend dendogram for gaps
library(cluster) # used to form clusters
library(clValid) # validating the results of clustering analysis
library(optCluster) # Determine Optimal Clustering Algorithm and Number of Clusters
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(GGally) # It extends 'ggplot2' by adding several functions to reduce complexity of geometric objects with transformed data
library(ggthemes) # It replicates the look of the plot
library(plotly) # makes interactive, publication-quality graphs
library(dplyr) # data manipulation

#Loading the dataset

creditcard = read_csv('D:/Semester-2-CourseWork/Assignment-DataSets/Task3/creditcard.csv') 
str(creditcard) # Structure of dataset
summary(creditcard)

# removing cust_id column and dropping na values, not affecting our analysis
creditcard <- creditcard %>% 
  select(-CUST_ID) %>% 
  drop_na()
head(creditcard) # top 5 records


# Data Visualization

# Correlation plot between variables
ggcorr(creditcard,label = T, label_size = 3, label_round = 2, hjust = 1, size = 3, color = "royalblue", layout.exp = 5, low = "dodgerblue", 
       mid = "gray95", high = "red2", name = "Correlation")

# 'Balance' variable plot

plot1 <-  ggplot(creditcard, aes(x=BALANCE)) +  geom_histogram(col = "black", fill = "lightgreen", bins = 30) +
  labs(x = "Balance", y = "Frequency", title = "Histogram of Customer Balance") +  theme_igray()
ggplotly(plot1)


# 'purchases' variable plot

plot2 <-  ggplot(creditcard, aes(x=PURCHASES)) +  geom_histogram(col = "grey", fill = "turquoise2", bins = 40) +
  labs(x = "Purchase", y = "Frequency", title = "Histogram of Customer Purchase") +  theme_igray()
ggplotly(plot2)


# 'credit limit' variable plot

plot3 <-  ggplot(creditcard, aes(x=CREDIT_LIMIT)) +  geom_histogram(col = "grey", fill = "violetred", bins = 30) +
  labs(x = "Credit Limit", y = "Frequency", title = "Histogram of Credit Limit") +  theme_igray()
ggplotly(plot3)


# 'Tenure' variable plot

plot4 <-  ggplot(creditcard, aes(x=TENURE)) +  geom_bar(col = "black", fill = "olivedrab") + 
  labs(x = "Tenure", y = "Frequency", title = "Bar Chart of Tenure") +  theme_igray()
ggplotly(plot4)


# Principal Component Analysis'

# scaling the dataset before PCA analysis
scaled.creditcard <- scale(creditcard)
summary(scaled.creditcard)

# finding Principal components
pca <- prcomp(scaled.creditcard) # using prcomp to standardize data between 0(mean) and 1(STD)
summary(pca)
pca

# Biplot of  PC1 and PC2

fviz_pca_biplot(pca, axes = c(1:2), col.var = "orange", col.ind = "royalblue", labelsize = 3) +  theme_igray() +
  labs(title = "Biplot of PC1 and PC2")

# Outliers of PC1 and PC2

fviz_pca_biplot(pca, axes = c(1:2), col.var = "orange", col.ind = "red", labelsize = 3, select.ind = list(contrib = 5)) +
  theme_igray() +  labs(title = "Outlier of PC1 and PC2")


# Outliers analysis

creditcard[c("465","513","1510","1167","2055"), ]

# Variable Data plot for PC1 and PC2

fviz_pca_var(pca, col.var="orange") +  theme_igray() +  labs(title = "Variables Factor Map - PC 1 & PC2")

# Dimensionality Reduction

credit_new <- pca$x[,1:12]
head(credit_new)

# Elbow Curve

fviz_nbclust(credit_new, kmeans, method = "wss", linecolor = "green4") +
  geom_vline(xintercept = c(3,7), linetype = 2, col = "red") +  theme_igray()


# K-Means 

# Forming 5 Clusters using loop function

for(i in 3:7){
  set.seed(1234)
  model <- kmeans(credit_new, i)
  print(paste("WSS of K",i, "=",model$tot.withinss))
  print(paste("BSS Proportion of K",i, "=", model$betweenss/model$totss))
  print(paste("Cluster Size of K",i, "="))
  print(paste(model$size))
  print(fviz_cluster(model, creditcard, palette = "Set1") +
          theme_igray())
}

# Fitting the K-means algorithm on Normalized Data with 6 Cluster Solution

fit <- kmeans(credit_new, 6)
fit_cluster <- fit$cluster

# Creating Data Frame by appending cluster membership on Original Dataset.

final <- data.frame(fit_cluster, creditcard)
View(final)

# creating a file in local system with clusters
write.csv(final,file = "D:/Semester-2-CourseWork/Assignment-DataSets/Task3/credit_cluster.csv",quote=TRUE,sep=",",col.names=NA)

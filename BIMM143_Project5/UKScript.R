#Importing the file and setting it as a variable 
UK <- read.csv("UK_foods.csv", header = TRUE, sep = ",")

#Viewing the file in a separate window 
View(UK)

#Checking dimensions of the file
dim(UK)

#Cleaning it up
rownames(UK) <- UK[ ,1]
UK <- UK[ ,-1]
View(UK)
dim(UK)

#Installing necessary packages
#install.packages("knitr")
library(knitr)

#Running function kable 
kable(UK, caption = "The full UK foods data table")

#Running a heatmap
par(mai = c(1,1,1,1))
heatmap(as.matrix(UK))

#Now using PCA
pca <- prcomp(t(UK))
summary(pca)

plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab = "PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(UK))

v <- round(pca$sdev^2/sum(pca$sdev^2) * 100)
v
z <- summary(pca)
z$importance

barplot(v, xlab = "Principal Component", ylab = "Percent Variation")

cumsum(v)

par(mar = c(10,3,3,2))
barplot(pca$rotation[,1], las=2)

biplot(pca)



---
title: "BIMM143 Class8"
author: "Jacob Gerzenshtein"
date: "April 26, 2018"
output:
  html_document:
    keep_md: yes
---



## Machine Learning with kmeans

Setting up some data  


```r
tmp <- c(rnorm(30, -3), rnorm(30,3))
x <- cbind (x = tmp, r = rev(tmp))

plot(x)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Using the kmeans() function 


```r
km <- kmeans(x, centers = 2, nstart = 20)
km
```

```
## K-means clustering with 2 clusters of sizes 30, 30
## 
## Cluster means:
##           x         r
## 1 -3.257845  3.178356
## 2  3.178356 -3.257845
## 
## Clustering vector:
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2
## [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
## 
## Within cluster sum of squares by cluster:
## [1] 57.82241 57.82241
##  (between_SS / total_SS =  91.5 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
```

Analyzing km 


```r
km$cluster
```

```
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2
## [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
```

```r
km$size
```

```
## [1] 30 30
```

```r
km$centers
```

```
##           x         r
## 1 -3.257845  3.178356
## 2  3.178356 -3.257845
```

Plotting it 


```r
plot(x, col = km$cluster)
points(km$centers, col = "Blue", pch = 12)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Doing it with k = 3


```r
km2 <- kmeans(x, centers = 3, nstart = 20)
km2 
```

```
## K-means clustering with 3 clusters of sizes 30, 16, 14
## 
## Cluster means:
##           x         r
## 1 -3.257845  3.178356
## 2  3.703047 -3.926250
## 3  2.578709 -2.493954
## 
## Clustering vector:
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 3 2 2 2
## [36] 2 2 2 3 3 3 3 3 3 2 3 2 3 2 2 3 2 3 3 2 2 3 3 2 2
## 
## Within cluster sum of squares by cluster:
## [1] 57.82241 11.15711 21.90875
##  (between_SS / total_SS =  93.3 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
```

Analyzing km3


```r
km2$cluster
```

```
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 3 2 2 2
## [36] 2 2 2 3 3 3 3 3 3 2 3 2 3 2 2 3 2 3 3 2 2 3 3 2 2
```

```r
km$size
```

```
## [1] 30 30
```

```r
km$centers
```

```
##           x         r
## 1 -3.257845  3.178356
## 2  3.178356 -3.257845
```
Plotting it


```r
plot(x, col = km2$cluster)
points(km$centers, col = "Blue", pch = 12)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## Machine Learning with Hierarchical clustering model  

Coding it: 


```r
dist_matrix <- dist(x)

#dist_matrix

hc <- hclust (d = dist_matrix)
```

Now looking at the distance matrix 


```r
class(dist_matrix)
```

```
## [1] "dist"
```


Looking at it 

```r
View(as.matrix(dist_matrix))
dim(as.matrix(dist_matrix))
```

```
## [1] 60 60
```


```r
plot(hc)
abline(h = 4, col = "red")
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
cutree(hc, k = 6)
```

```
##  [1] 1 1 1 2 3 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 3 1 3 3 1 3 1 1 4 4 5 4 5
## [36] 5 4 5 4 4 4 4 4 4 5 4 4 4 4 4 4 4 4 4 4 5 6 4 4 4
```


```r
plot(x, col = cutree(hc, k = 5))
points(km2$centers, col = "blue", pch = 12)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
print(km2$centers)
```

```
##           x         r
## 1 -3.257845  3.178356
## 2  3.703047 -3.926250
## 3  2.578709 -2.493954
```

Different clusters: 


```r
#hc.complete <- hclust(x, method = "complete")
```


```r
x <- rbind(
  matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
  matrix(c(rnorm(50, mean = 1, sd = 0.3),
          rnorm(50, mean = 0, sd = 0.3)), ncol = 2))

colnames(x) <- c("x", "y")

plot(x)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
col <- as.factor(rep(c("c1", "c2", "c3"), each = 50))

plot(x, col = col)
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
plot (x, col=cutree(hc, k = 3))
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-14-3.png)<!-- -->



```r
x <- rbind(
  matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
  matrix(c(rnorm(50, mean = 1, sd = 0.3),
          rnorm(50, mean = 0, sd = 0.3)), ncol = 2))


matrix2 <- dist(x)

hc2 <- hclust (d = matrix2)

plot(hc2)

abline(h = 2, col = "red")
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
cutree(hc2, k = 3)
```

```
##   [1] 1 2 1 1 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2
##  [36] 2 2 2 2 2 2 2 1 1 2 2 2 1 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
##  [71] 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 1 3 1
## [106] 1 3 3 3 3 1 1 1 3 1 3 1 1 2 1 3 1 1 3 1 1 1 1 3 3 1 3 1 1 1 1 3 3 1 1
## [141] 1 1 3 1 1 3 1 1 3 1
```

```r
plot(x, col = cutree(hc2, k = 3))
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

## PCA time 


```r
mydata <- matrix(nrow = 100, ncol = 10)
rownames(mydata) <- paste("gene", 1:100, sep = "")
colnames(mydata) <- c(paste("wt", 1:5, sep = ""),
                      paste("ko", 1:5, sep = ""))
for (i in 1:nrow(mydata)) {
  wt.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))
  ko.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))
  
  mydata[i, ] <- c(wt.values, ko.values)
  
}

head(mydata)
```

```
##       wt1 wt2 wt3 wt4 wt5 ko1 ko2 ko3 ko4 ko5
## gene1 597 593 597 573 560 547 528 540 511 529
## gene2 495 466 477 515 514 431 453 460 463 433
## gene3 759 790 712 786 777 226 234 245 238 239
## gene4  41  43  38  51  45 947 929 987 960 926
## gene5 884 865 909 873 881 390 367 368 389 408
## gene6 211 197 202 195 208 883 874 881 857 880
```

```r
pca <- prcomp(t(mydata), scale = TRUE)

attributes(pca)
```

```
## $names
## [1] "sdev"     "rotation" "center"   "scale"    "x"       
## 
## $class
## [1] "prcomp"
```

```r
plot(pca$x[ ,1], pca$x [ ,2])
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
pca.var <- pca$sdev^2

pca.var.per <- round(pca.var/sum(pca.var) * 100, 1)

pca.var.per
```

```
##  [1] 90.7  2.3  1.8  1.5  1.1  0.9  0.8  0.4  0.3  0.0
```

```r
barplot(pca.var.per, main = "Scree Plot", 
        xlab = "Principal Component", ylab = "Percent Variation")
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```r
colvec <- colnames(mydata)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"


plot(pca$x[ ,1], pca$x[ ,2], col = colvec, pch = 16,
     xlab = paste0("PC1 (", pca.var.per[1], "%)"),
     ylab = paste0("PC2 (", pca.var.per[2], "%)"))
```

![](BIMM143_Class8_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

```r
# run in console to identify points:
# identify(pca$x[ ,1], pca$x[ ,2], labels = colnames(mydata))
```



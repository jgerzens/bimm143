---
title: "Bioinformatics Class 7"
author: "Jacob Gerzenshtein"
date: "April 24, 2018"
output:
  html_document:
    keep_md: yes
---




## Functions Again

Here we are going to revisit our function from class 6


```r
source("http://tinyurl.com/rescale-R")
```

Let's see if we can use this function



```r
rescale(1:10)
```

```
##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
##  [8] 0.7777778 0.8888889 1.0000000
```


Looks good - let's break it 


```r
#rescale(c(1:10, "string"))
```

Let's try the new **rescale2** function


```r
#rescale2(c(1:10, "string"))
```

## Writing a NA checking function

Here we write a new function to check for NA's in two inputs


```r
x <- c(1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
```


```r
which(is.na(x))
```

```
## [1] 3 5
```

```r
which(is.na(y))
```

```
## [1] 1 3
```


```r
sum (is.na(x))
```

```
## [1] 2
```


```r
is.na(x)
```

```
## [1] FALSE FALSE  TRUE FALSE  TRUE
```

```r
is.na(y)
```

```
## [1]  TRUE FALSE  TRUE FALSE FALSE
```



```r
is.na(x) & is.na(y)
```

```
## [1] FALSE FALSE  TRUE FALSE FALSE
```



Let's out this together with x and y 


```r
sum(is.na(x) & is.na(y))
```

```
## [1] 1
```

## The function:

```r
both_na <- function(x, y) {



sum(is.na(x) & is.na(y))

}
```

Test it


```r
both_na(x,y)
```

```
## [1] 1
```


Breaking it: 


```r
x <- c(NA, NA, NA)
y1 <- c(1, NA, NA)
y2 <- c(1, NA, NA, NA)

both_na(x,y)
```

```
## Warning in is.na(x) & is.na(y): longer object length is not a multiple of
## shorter object length
```

```
## [1] 2
```

New function to create warning:


```r
both_na2 <- function (x,y) {
  
  
  if (length(x) != length(y)) {
    
    stop("Input x and y should be the same length")
    
  }
  
  sum(is.na(x) & is.na(y))
  
}
```

Test it again in console 


A new function to find number of matches and where they are:


```r
both_na3 <- function(x,y) {
  
  if (length(x) != length(y)) { 
    
    stop("Input x and y should be vectors of the same length")
    
    }
  
  na.in.both <- (is.na(x) & is.na(y))
  na.number <- sum(na.in.both)
  na.which <- which(na.in.both)
  
  message("Found", na.number, "NA's at position(s):", paste(na.which, collapse = ", "))
  
  
  return(list(number=na.number, which=na.which))
  
  
}
```

Try it: 


```r
x <- c(1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
both_na3(x,y)
```

```
## Found1NA's at position(s):3
```

```
## $number
## [1] 1
## 
## $which
## [1] 3
```





## Follow along: a different function for gene intersection 


Sourcing the code 

```r
source("http://tinyurl.com/rescale-R")
```


```r
df1
```

```
##     IDs exp
## 1 gene1   2
## 2 gene2   1
## 3 gene3   1
```

```r
df2
```

```
##     IDs exp
## 1 gene2  -2
## 2 gene4  NA
## 3 gene3   1
## 4 gene5   2
```

```r
x <- df1$IDs
y <- df2$IDs
```


```r
x
```

```
## [1] "gene1" "gene2" "gene3"
```

```r
y
```

```
## [1] "gene2" "gene4" "gene3" "gene5"
```


Seeing where there are similarities 

```r
intersect (x,y)
```

```
## [1] "gene2" "gene3"
```

```r
x%in%y
```

```
## [1] FALSE  TRUE  TRUE
```

```r
y%in%x
```

```
## [1]  TRUE FALSE  TRUE FALSE
```

Now access the genes with desired indices 


```r
x[x%in%y]
```

```
## [1] "gene2" "gene3"
```

```r
y[y%in%x]
```

```
## [1] "gene2" "gene3"
```


Now put them together with **cbind()**


```r
cbind(x[x%in%y], y[y%in%x])
```

```
##      [,1]    [,2]   
## [1,] "gene2" "gene2"
## [2,] "gene3" "gene3"
```



```r
gene_intersect <- function (df1, df2) {

cbind(df1[df1$IDs %in% df2$IDs, ],
      df2[df2$IDs %in% df1$IDs, "exp"])

}
```


```r
gene_intersect2(df1, df2)
```

```
##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
## 2 gene2   1                               -2
## 3 gene3   1                                1
```




```r
merge(df1, df2, by = "IDs")
```

```
##     IDs exp.x exp.y
## 1 gene2     1    -2
## 2 gene3     1     1
```


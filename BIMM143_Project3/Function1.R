#' ---
#' title: "BIMM 143 Functions"
#' author: "Jacob Gerzenshtein"
#' date: "Spring 2018"
#' ---

#Class 6 BIMM 143

#Functions

add <- function(x, y = 1) {
  
  #Sum x and y
  
  x + y
  
}

rescale <- function(x) {
  
  rng <- range(x, na.rm = TRUE)
  
  (x-rng[1]) / (rng[2] - rng[1])
  
}

rescale2 <- function(x, na.rm = TRUE, plot = FALSE) {
  if (na.rm) {
    rng <- range (x, na.rm = na.rm)
  } else {
    rng <- range(x)
  }
  print("Hello")
  
  answer <- (x-rng[1]) / (rng[2] - rng[1])
  
  print("is it me you are looking for?")
  
  if (plot) {
    plot(answer, typ = "b", lwd = 4)
  }
  
  print("I can see it in your eyes")
  
}


rescale3 <- function(x, na.rm = TRUE, plot = FALSE) {
  if (na.rm) {
    rng <- range (x, na.rm = na.rm)
  } else {
    rng <- range(x)
  }
  print("Hello")
  
  answer <- (x-rng[1]) / (rng[2] - rng[1])
  
  
print("is it me you are looking for?")
  
  if (plot) {
    plot(answer, typ = "h", lwd = 4)
  }
  
  print("I can see it in your eyes")
  
  return(answer)
}

# (A. Can you improve this code?)

df <- data.frame(a=1:10, b = seq(200, 400, length = 10), c = 11:20, d = NA)

df$a <- (df$a - min(df$a)) / (max(df$a) - min(df$a))
df$b <- (df$b - min(df$b)) / (max(df$b) - min(df$b))
df$a <- (df$c - min(df$c)) / (max(df$c) - min(df$c))
df$a <- (df$d - min(df$d)) / (max(df$d) - min(df$d))

#install.packages("bio3d")

library(bio3d)

# Can you improve this analysis code?

#s1 <- read.pdb("4AKE")
#s2 <- read.pdb("1AKE")
#s3 <- read.pdb("1E4Y")

#s1.chainA <- trim.pdb(s1, chain = "A", elety = "CA")
#s2.chainA <- trim.pdb(s2, chain = "A", elety = "CA")
#s3.chainA <- trim.pdb(s3, chain = "A", elety = "CA")

#s1.b <- s1.chainA$atom$b
#s2.b <- s2.chainA$atom$b
#s3.b <- s3.chainA$atom$b

#plotb3(s1.b, sse = s1.chainA, typ = "l", ylab = "BFactor")
#plotb3(s2.b, sse = s2.chainA, typ = "l", ylab = "BFactor")
#plotb3(s3.b, sse = s3.chainA, typ = "l", ylab = "BFactor")

# showmewhatyouplot = function(x, y, z) {
#   
#   s1 <- read.pdb(x)
#   s2 <- read.pdb(y)
#   s3 <- read.pdb(z)
#   x1 <- trim.pdb(s1, chain = "A", elety = "CA")
#   x2 <- trim.pdb(s2, chain = "A", elety = "CA")
#   x3 <- trim.pdb(s3, chain = "A", elety = "CA")
#   df1 <- x1$atom$b
#   df2 <- x2$atom$b
#   df3 <- x3$atom$b
#   p1 <- plotb3(df1, sse = x1, type = "p", ylab = "BFactor")
#   p2 <- plotb3(df2, sse = x2, type = "p", ylab = "BFactor")
#   p3 <- plotb3(df3, sse = x3, type = "p", ylab = "BFactor")
#  
#   hc <-  hclust(dist(rbind(df1, df2, df3)))
#   
#   plot(hc)
#   
# }

# showmewhatyouplot2 <- function(x) {
#  
#   r <- read.pdb(x)
#   t <- trim.pdb(r, chain = "A", elety = "CA")
#   df <- t$atom$b
#   p <- plotb3(df, sse = t, type = "l", ylab = "B Factor")
#    
# }

genecluster <- function(x, y, z) {

    s1 <- read.pdb(x)
    s2 <- read.pdb(y)
    s3 <- read.pdb(z)
    x1 <- trim.pdb(s1, chain = "A", elety = "CA")
    x2 <- trim.pdb(s2, chain = "A", elety = "CA")
    x3 <- trim.pdb(s3, chain = "A", elety = "CA")
    df1 <- x1$atom$b
    df2 <- x2$atom$b
    df3 <- x3$atom$b

    hc <-  hclust(dist(rbind(df1, df2, df3)))

    plot(hc, xlab = "Test Label")
}


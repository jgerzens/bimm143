#x <- data.frame(1:10, 11:20)
#y <- kmeans(x, center = 2, nstart = 10)
#plot(y)

tmp <- c(rnorm(30, -3), rnorm(30,3))
x <- cbind (x = tmp, r = rev(tmp))

plot(x)

kmeans(x, centers = 2, nstart = 20)




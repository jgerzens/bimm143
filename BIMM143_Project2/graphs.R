#' ---
#' title: "Bioinformatics BIMM 143 Class 5"
#' author: "Jacob Gerzenshtein"
#' output:
#'   html_document:
#'      code_folding: hide
#' ---

# Class 5 project (graphs)

#boxplot

boxplot(rnorm(1000,0), horizontal = TRUE, boxwex = 0.5) 

#histogram

hist(rnorm(1000,0))

summary(rnorm(1000,0))

#lab plot 1

baby_plot <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)

View(baby_plot)

plot(baby_plot, type = "b", 
     pch = 15, 
     cex = 1.5, 
     lwd = 2, 
     ylim = c(2,10), 
     xlab = "Age (months)", 
     ylab = "Weight (kg)", 
     main = "Baby", 
     col = rainbow(7))

#lab plot 2

feature_plot <- read.table("bimm143_05_rstats/feature_counts.txt", 
                           header = TRUE, 
                           sep = "\t")

View(feature_plot)

class(feature_plot)

par(mar = c(11,11,11,11))

barplot(feature_plot[ ,2], horiz = TRUE, ylab = "A Title", 
        names.arg = feature_plot[ ,1], 
        main = "Some Title", 
        las = 1,
        col = rainbow(7))


#lab part 2


mfcount <- read.table("bimm143_05_rstats/male_female_counts.txt", 
                      sep = "\t", 
                      header = TRUE)

View(mfcount)

barplot(mfcount$Count,
        col = c("blue2", "red2")
        )

updown <- read.table("bimm143_05_rstats/up_down_expression.txt",
                     #sep = "\t",
                     header = TRUE)

View(updown)

levels(updown$State)

palette(c("blue","grey", "red"))

par(mar = (c(5,5,5,5)))

plot(updown$Condition1, updown$Condition2, 
     col = updown$State,
     xlab = "Condition 1",
     ylab = "Condition 2")

levels(updown$State)

colorval <- read.table("bimm143_05_rstats/expression_methylation.txt", 
                       header = TRUE)

View(colorval)

map.colors <- function (value,high.low,palette) {
  
  #Determine percent values of the high.low range
  proportion <- ((value-high.low[1])/(high.low[2]-high.low[1]))
  
  #index <- round( (length(palette) - 1) * percent) + 1
  index <- round ((length(palette)-1)*proportion)+1
  
  return (palette[index])
}

colorRampPalette(c("grey", "red"))

colorlevel <- colorRampPalette(c("grey", "red"))(100)

high <- max(colorval$expression)
low <- min(colorval$expression)
highlow <- c(high, low)

plot(colorval$promoter.meth, 
     colorval$gene.meth, 
     col = map.colors(colorval$expression, highlow, colorlevel),
     xlab = "Promoter Methylation",
     ylab = "Gene Methylation")

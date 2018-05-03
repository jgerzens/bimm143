wisc.df <- read.csv("WisconsinCancer.csv")

View(wisc.df)

wisc.data <- as.matrix(wisc.df)

row.names(wisc.data) <- wisc.df$id

diagnosis <- as.numeric(wisc.df$diagnosis == "M")

table(diagnosis)

table(wisc.df$diagnosis)


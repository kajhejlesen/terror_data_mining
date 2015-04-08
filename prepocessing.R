library(data.table)

data_as_frame <- read.csv(file = "data/terror.csv")
data <- as.data.table(data_as_frame)

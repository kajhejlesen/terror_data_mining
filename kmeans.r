library(data.table)
library(rworldmap)
library(RColorBrewer)




kmeans_countries <- function(data, att1, useNA = "no", centers = 9, iter.max = 1000, nstart = 10) {
  by_target <- as.data.table(table(data[["country_txt"]], data[[att1]], useNA = useNA))
  setnames(by_target, old = colnames(by_target), new = c("country_txt", att1, "sum"))
  wide_format <- reshape(by_target, timevar = att1, idvar = c("country_txt"), direction = "wide")
  
  result_matrix <- data.matrix(wide_format)
  
  result_matrix <- result_matrix[,-1]
  
  #normalizing matrix by row (country)
  result_matrix_norm <- t(apply(result_matrix, 1, scale))
  
  # running k-means and associating each country to a cluster in text
  clusters <- kmeans(result_matrix_norm, centers = centers, iter.max = iter.max, nstart = nstart)
  
  countries <- as.data.table(cbind(wide_format$country_txt, clusters$cluster))
  countries$V2 <- as.numeric(countries$V2)
  setorder(countries, V2)
  clusters$countries <- countries
  
  # normalizing centers based on mean/sd of all countries, storing in $dev
  means <- apply(result_matrix_norm, 2, mean)
  sds <- apply(result_matrix_norm, 2, sd)
  clusters$dev <- sweep(clusters$centers, 2, means, "-")
  clusters$dev <- sweep(clusters$dev, 2, sds, "/")
  
  # generating map
  clusters$map <- joinCountryData2Map(countries, joinCode = "NAME", nameJoinColumn = "V1")
  
  clusters
}

#removing 'other' and 'unknown' from dataset
data_filtered <- data[!data$targtype1 %in% c(13,20)]
#data_filtered <- data[data$nkill > 0]

# removing countries with fewer than 20 entries (71 in total)
country_counts <- data.frame(table(data_filtered$country_txt))
low_data_countries <- country_counts$Var1[country_counts$Freq < 15]
data_filtered <- data_filtered[!data_filtered$country_txt %in% low_data_countries]

data_filtered$country_txt <- factor(data_filtered$country_txt)

# k-means for 138 countries
clusters <- kmeans_countries(data_filtered, "targtype1", centers = 8, iter.max = 1000, nstart = 1000)

mapCountryData(mapToPlot = clusters$map, nameColumnToPlot = "V2", numCats = 8, 
               catMethod = "categorical", colourPalette = brewer.pal(8, "Set1"),
               missingCountryCol = "grey90", oceanCol = "lightsteelblue1", addLegend = T)


#removing 'other' and 'unknown' from dataset
data_filtered <- data[!data$attacktype1 %in% c(9)]

# removing countries with fewer than 20 entries (71 in total)
country_counts <- data.frame(table(data_filtered$country_txt))
low_data_countries <- country_counts$Var1[country_counts$Freq < 15]
data_filtered <- data_filtered[!data_filtered$country_txt %in% low_data_countries]

data_filtered$country_txt <- factor(data_filtered$country_txt)

# k-means for 138 countries
clusters <- kmeans_countries(data_filtered, "attacktype1", centers = 8, iter.max = 1000, nstart = 1000)

mapCountryData(mapToPlot = clusters$map, nameColumnToPlot = "V2", numCats = 8, 
               catMethod = "categorical", colourPalette = brewer.pal(8, "Set1"),
               missingCountryCol = "grey90", oceanCol = "lightsteelblue1", addLegend = T)

order(clusters$dev[4,], decreasing=T)

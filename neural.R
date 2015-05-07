library(neuralnet)
library(nnet)
library(devtools)
library(data.table)

country_counts <- data.frame(table(data$country_txt))
low_data_countries <- country_counts$Var1[country_counts$Freq < 15]
data_filtered <- data[!data$country_txt %in% low_data_countries]

data_filtered$country_txt <- factor(data_filtered$country_txt)

countries <- levels(data_filtered$country_txt)


generate_input_frame <- function(data, start, end, by) {
  total <- data.frame()
  
  for (year in seq(from = start,to = end, by = by)) {
    data_test <- subset(data, subset = iyear >= year & iyear < year+5)
    data_result <- subset(data, subset = iyear >= year+5 & iyear < year+7)
    input <- data.table(as.data.frame(table(data_test$country_txt, data_test$iyear, data_test$attacktype1_txt)))
    input <- input[, j=sum(Freq), by = list(Var1, Var3)]
    input$start_year <- year
    input <- reshape(input, timevar = c("Var3"), idvar = c("Var1","start_year"), direction = "wide")
    
    
    result <- data.frame(table(data_result$country_txt, data_result$nkill > 10))
    result$is_true <- ifelse(test = result$Var2 == TRUE & result$Freq > 0, 1,0)
    result <- result[152:nrow(result),]
    
    input$result <- res$is_true
    
    total <- rbind(total, input)
    
  }
  setnames(total, colnames(total), c("country", "start_year", "x1","x2","x3","x4","x5","x6","x7","x8","x9","result"))
  
  total
}


training_set <- generate_input_frame(data_filtered, 1970,2006,4)
test_set <- generate_input_frame(data_filtered, 1972,2004,4)


form.in<-as.formula('result~x1+x2+x3+x4+x5+x6+x7+x8+x9')

mod2<-neuralnet(form.in,data=training_set,hidden= c(10), linear.output = FALSE, lifesign="minimal", lifesign.step = 100, stepmax = 1E6)

plot(mod2)


prediction <- compute(mod2, test_set[,3:11, with = FALSE])

test_set$prediction <- prediction


result_name <- as.data.frame(cbind(countries, as.vector(mod2$net.result[[1]])))
result_name$V2 <- as.numeric(as.character(result_name$V2))

map <- joinCountryData2Map(result_name, joinCode = "NAME", nameJoinColumn = "countries")

mapCountryData(mapToPlot = map, nameColumnToPlot = "V2", numCats = 9,
               catMethod = "diverging", colourPalette = brewer.pal(9, "Reds"),
               missingCountryCol = "grey90", oceanCol = "lightsteelblue1", addLegend = T)

mod2$
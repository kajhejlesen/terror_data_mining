library(neuralnet)
library(nnet)
library(devtools)
library(data.table)

country_counts <- data.frame(table(data$country_txt))
low_data_countries <- country_counts$Var1[country_counts$Freq < 15]
data_filtered <- data[!data$country_txt %in% low_data_countries]

data_filtered$country_txt <- factor(data_filtered$country_txt)

countries <- levels(data_filtered$country_txt)

# alternative normalization function, not used
norm.fun = function(x){
  (x - min(x))/(max(x) - min(x)) 
}



total <- data.frame()
time_series <- seq(from = 1970, to = 2006, by = 2)

# removing result that contain 1993 as we have no data for that year
time_series <- time_series[!time_series %in% c(1988)]

for (year in time_series) {
  
  data_test <- subset(data_filtered, subset = iyear >= year & iyear < year+5)
  data_result <- subset(data_filtered, subset = iyear >= (year+5) & iyear < (year+7))
  input <- data.table(as.data.frame(table(data_test$country_txt, data_test$iyear, data_test$attacktype1_txt)))
  input <- input[, j=sum(Freq), by = list(Var1, Var3)]
  input$start_year <- year
  input <- reshape(input, timevar = c("Var3"), idvar = c("Var1","start_year"), direction = "wide")
  
  # generating data for average fatalities per incident
  kill_rate_country <- data_test[, j=mean(nkill, na.rm=TRUE), by = list(country_txt)]
  data_test.keys <- data_filtered[,CJ(country_txt=unique(country_txt))]
  setkey(kill_rate_country, country_txt)
  kill_rate_country <- kill_rate_country[data_test.keys]
  kill_rate_country[is.na(kill_rate_country)] <- 0
  input$killrate <- kill_rate_country$V1
  
  
  
  result <- data.frame(table(data_result$country_txt, data_result$nkill > 10))
  result$is_true <- ifelse(test = result$Var2 == TRUE & result$Freq > 0, 1,0)
  result <- result[152:nrow(result),]
  
  input$result <- res$is_true
  
  
  total <- rbind(total, input)  
}

setnames(total, colnames(total), c("country", "start_year", "Assassination","Hijacking","Kidnapping","Barricade","Bombing","Unknown","Armed.Assault","Unarmed.Assault","Facility","Kill.Rate", "result"))


data_set <- data.table(apply(total[,c(4:length(total)-1), with=FALSE], 2, scale))
data_set$result <- total$result

trainingsamples <- sample(1:nrow(data_set), nrow(data_set)/2)

form.in<-as.formula('result~Assassination + Hijacking + Kidnapping + Barricade + Bombing + Unknown + Armed.Assault + Unarmed.Assault + Facility + Kill.Rate')

mod2<-neuralnet(form.in,data=data_set[trainingsamples],hidden= c(8), linear.output = FALSE, lifesign="minimal", lifesign.step = 100, stepmax = 1E5, rep = 5)
plot(mod2)

test_set <- data_set[!trainingsamples]

prediction <- compute(mod2, test_set[,1:10, with = FALSE])

test_set$prediction <- prediction$net.result
test_set$prediction.round <- ifelse(test_set$prediction >= 0.5, 1,0)

confusion_matrix <- table(test_set$prediction.round, test_set$result)

confusion_matrix

plot.nnet(mod2)

result_name <- as.data.frame(cbind(countries, as.vector(mod2$net.result[[1]])))
result_name$V2 <- as.numeric(as.character(result_name$V2))

map <- joinCountryData2Map(result_name, joinCode = "NAME", nameJoinColumn = "countries")

mapCountryData(mapToPlot = map, nameColumnToPlot = "V2", numCats = 9,
               catMethod = "diverging", colourPalette = brewer.pal(9, "Reds"),
               missingCountryCol = "grey90", oceanCol = "lightsteelblue1", addLegend = T)

